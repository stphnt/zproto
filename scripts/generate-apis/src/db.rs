//! Types and functions for querying the database.

use crate::{AsciiVariant, Enum, EnumVariant, ParamType, Scope, Version};
use rusqlite::Connection;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::path::Path;

/// Get read-only connection to the database `path`.
pub fn get_connection(path: impl AsRef<Path>) -> anyhow::Result<Connection> {
	use rusqlite::OpenFlags;

	Ok(Connection::open_with_flags(
		path,
		OpenFlags::SQLITE_OPEN_READ_ONLY,
	)?)
}

pub type AsciiSettings = BTreeMap<String, (Scope, BTreeMap<AsciiVariant, BTreeSet<Version>>)>;

/// All data we care about, extracted from the database.
#[derive(Debug)]
pub struct Data {
	pub ascii_settings: AsciiSettings,
	pub types: BTreeMap<u32, ParamType>,
	pub enums: BTreeMap<u32, Enum>,
	pub versions: Vec<Version>,
}

impl Data {
	/// Create a new instance of [`Data`] from a database connection.
	///
	/// The instance will be populated with all the data we care about.
	pub fn new(conn: &mut Connection) -> anyhow::Result<Self> {
		let versions = Self::get_versions(conn)?;
		let ascii_settings = Self::get_ascii_settings(conn)?;
		Ok(Self {
			versions,
			ascii_settings,
			types: Self::get_types(conn)?,
			enums: Self::get_enums(conn)?,
		})
	}

	/// Get a list of all relevant firmware versions in the database.
	fn get_versions(conn: &mut Connection) -> anyhow::Result<Vec<Version>> {
		log::info!("loading versions ...");
		let mut statement = conn.prepare(
			"SELECT Major, Minor
			FROM Data_Versions
			WHERE Minor < 95
			AND (Major, Minor) >= (6, 14)
			ORDER BY Major, Minor",
		)?;
		let versions = statement
			.query_map([], |row| {
				Ok(Version {
					major: row.get(0)?,
					minor: row.get(1)?,
				})
			})?
			.collect::<Result<Vec<_>, _>>()?;
		Ok(versions)
	}

	fn get_ascii_settings(conn: &mut Connection) -> anyhow::Result<AsciiSettings> {
		log::info!("loading ASCII settings ...");

		// Get the names of settings associated with a peripheral, meaning they are axis scope.
		// cSpell:ignore ASV ASVG ASVSG
		let axis_scope_setting_names: HashSet<String> = {
			let mut statement = conn.prepare(
				"SELECT SC.Name
				FROM Data_SettingsCommon SC
				WHERE EXISTS (
					SELECT 1
					FROM Data_ProductAttributes PA
					INNER JOIN Data_AsciiSettingValueGroups ASVG ON ASVG.Id = PA.AsciiSettingValueGroupId
					INNER JOIN Data_AsciiSettingValueSubgroups ASVSG ON ASVSG.Id = ASVG.MemberId
					INNER JOIN Data_AsciiSettingValues ASV ON ASV.Id = ASVSG.MemberId
					INNER JOIN Data_AsciiSettings AS_ ON AS_.Id = ASV.AsciiSettingId
					WHERE PA.ParentProductGroupId IS NOT NULL
					AND SC.Id = AS_.SettingId
				)",
			)?;
			let rows = statement.query_and_then([], |row| -> rusqlite::Result<_> { row.get(0) })?;
			rows.collect::<Result<_, _>>()?
		};

		// Collect the actual setting information
		let setting_info: BTreeMap<_, _> = {
			let mut statement = conn.prepare(
				"SELECT Id, TypeId, EnumTypeId, Name
				FROM Data_SettingsCommon
				WHERE Name IS NOT NULL",
			)?;
			let rows = statement.query_and_then([], |row| -> rusqlite::Result<_> {
				let id: u32 = row.get(0)?;
				let param_type: u32 = row.get(1)?;
				let enum_type: Option<u32> = row.get(2)?;
				let name: String = row.get(3)?;
				Ok((
					id,
					(
						AsciiVariant {
							param_type,
							enum_type,
						},
						name,
					),
				))
			})?;
			rows.collect::<Result<_, _>>()?
		};

		// Get version information for each setting
		let mut statement = conn.prepare(
			"SELECT SC.Id, V.Major, V.Minor
			FROM Data_ProductAttributes PA
			INNER JOIN Data_AsciiSettingValueGroups ASVG ON ASVG.Id = PA.AsciiSettingValueGroupId
			INNER JOIN Data_AsciiSettingValueSubgroups ASVSG ON ASVSG.Id = ASVG.MemberId
			INNER JOIN Data_AsciiSettingValues ASV ON ASV.Id = ASVSG.MemberId
			INNER JOIN Data_AsciiSettings AS_ ON AS_.Id = ASV.AsciiSettingId
			INNER JOIN Data_SettingsCommon SC ON SC.Id = AS_.SettingId
			INNER JOIN Data_VersionGroups VG ON VG.Id = PA.VersionGroupId
			INNER JOIN Data_Versions V ON V.Id = VG.MemberId
			WHERE (V.Major, V.Minor) >= (6, 14)
			AND V.Minor < 95",
		)?;
		let iter = statement.query_map((), |row| {
			Ok((
				row.get::<_, u32>(0)?,
				Version {
					major: row.get(1)?,
					minor: row.get(2)?,
				},
			))
		})?;

		let mut settings = BTreeMap::<String, _>::default();
		for result in iter {
			let (setting_id, version) = result?;
			let (variant, name) = setting_info.get(&setting_id).unwrap();
			let setting_entry = settings.entry(name.clone()).or_insert_with(|| {
				(
					if axis_scope_setting_names.contains(name) {
						Scope::Axis
					} else {
						Scope::Device
					},
					BTreeMap::default(),
				)
			});
			let versions: &mut BTreeSet<Version> = setting_entry.1.entry(*variant).or_default();
			versions.insert(version);
		}

		// Check that each setting only has one variant per version.
		let mut passed = true;
		let mut found_versions = HashSet::new();
		for (setting, (_scope, variants)) in &mut settings {
			found_versions.clear();
			for versions in variants.values() {
				for version in versions {
					if found_versions.contains(version) {
						log::error!("{setting} has multiple variants in {version}");
						passed = false;
					} else {
						found_versions.insert(*version);
					}
				}
			}
		}
		if !passed {
			panic!("some settings have multiple variants in one version");
		}

		Ok(settings)
	}

	/// Get a mapping of all the data types, keyed by parameter ID.
	fn get_types(conn: &mut Connection) -> anyhow::Result<BTreeMap<u32, ParamType>> {
		log::info!("loading types ...");
		let mut statement = conn.prepare("SELECT Id, Name FROM Data_Types;")?;
		let rows = statement.query_and_then([], |row| -> rusqlite::Result<_> {
			Ok((row.get(0)?, ParamType { name: row.get(1)? }))
		})?;
		let mut types = BTreeMap::default();
		for row in rows {
			let (id, param_type) = row?;
			types.insert(id, param_type);
		}
		Ok(types)
	}

	/// Get a mapping of enums, keyed by enum ID.
	fn get_enums(conn: &mut Connection) -> anyhow::Result<BTreeMap<u32, Enum>> {
		log::info!("loading enums ...");
		let mut statement = conn.prepare(
			"SELECT
				Type.Id,
				Type.Name,
				Type.Description,
				Var.Name,
				Var.Description
			FROM Data_EnumTypes AS Type
			JOIN Data_EnumMembers AS Var ON Var.EnumTypeId = Type.Id;",
		)?;
		type Record = (u32, String, Option<String>, String, Option<String>);
		let rows = statement.query_and_then([], |row| -> rusqlite::Result<Record> {
			Ok((
				row.get(0)?,
				row.get(1)?,
				row.get(2)?,
				row.get(3)?,
				row.get(4)?,
			))
		})?;
		let mut enums = BTreeMap::default();
		for row in rows {
			let (id, name, description, var_name, var_description) = row?;
			let entry = enums.entry(id).or_insert_with(|| Enum {
				name,
				description,
				variants: Vec::new(),
			});
			entry.variants.push(EnumVariant {
				name: var_name,
				description: var_description,
			});
		}
		Ok(enums)
	}

	/// Get the name for a setting's type for use in Rust source code.
	///
	/// This is most often defined by the param type or enum type of the variant.
	/// However, for some settings we explicitly override the type.
	pub fn rust_setting_value_type_name(&self, name: &str, variant: &AsciiVariant) -> String {
		if name == "version" {
			return "crate::ascii::data_types::Version".to_string();
		}
		let param_type = self
			.types
			.get(&variant.param_type)
			.unwrap_or_else(|| panic!("invalid param type id {}", variant.param_type));
		if param_type.is_enum() {
			let enum_type_id = variant.enum_type.unwrap();
			let enum_type = self.enums.get(&enum_type_id).unwrap();
			enum_type.rust_type_name()
		} else {
			param_type
				.rust_type_name()
				.map(|name| name.to_string())
				.unwrap()
		}
	}
}
