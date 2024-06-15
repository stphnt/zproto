//! Types and functions for querying the database.

use crate::{AsciiVariant, Enum, EnumVariant, ParamType, Scope, Version};
use fnv::{FnvHashMap, FnvHashSet};
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

pub type AsciiSettings = BTreeMap<String, (Scope, FnvHashMap<AsciiVariant, BTreeSet<Version>>)>;

/// All data we care about, extracted from the database.
#[derive(Debug)]
pub struct Data {
	pub ascii_settings: AsciiSettings,
	pub param_types: FnvHashMap<u32, ParamType>,
	pub enums: FnvHashMap<u32, Enum>,
	pub versions: Vec<Version>,
}

impl Data {
	/// Create a new instance of [`Data`] from a database connection.
	///
	/// The instance will be populated with all the data we care about.
	pub fn new(conn: &mut Connection) -> anyhow::Result<Self> {
		let versions = Self::get_versions(conn)?;
		let ascii_settings = Self::get_ascii_settings(conn, &versions)?;
		Ok(Self {
			versions,
			ascii_settings,
			param_types: Self::get_param_types(conn)?,
			enums: Self::get_enums(conn)?,
		})
	}

	/// Get a list of all relevant firmware versions in the database.
	fn get_versions(conn: &mut Connection) -> anyhow::Result<Vec<Version>> {
		let mut statement = conn.prepare(
			r#"SELECT DISTINCT MajorVersion, MinorVersion FROM Data_Products
			WHERE MajorVersion IS NOT NULL
			AND MinorVersion IS NOT NULL;"#,
		)?;
		let rows = statement.query_map([], |row| {
			Ok(Version {
				major: row.get(0)?,
				minor: row.get(1)?,
			})
		})?;
		let mut versions = Vec::new();
		for row in rows {
			let version = row?;
			if version >= (6, 14) && version.minor < 98 {
				versions.push(version);
			}
		}
		versions.sort();
		Ok(versions)
	}

	fn get_related_setting_ids(
		conn: &mut Connection,
		version: Version,
	) -> anyhow::Result<Vec<u32>> {
		let mut statement = conn.prepare(
			"SELECT DISTINCT SettingId 
			FROM Data_ProductsSettings 
			WHERE ProductId IN (
				SELECT Id FROM Data_Products
				WHERE MajorVersion = ?
				AND MinorVersion = ?
				UNION
				SELECT Child.Id FROM Data_Products AS Child
				JOIN Data_Products AS Parent ON Parent.Id = Child.ParentId
				WHERE Parent.MajorVersion = ?
				AND Parent.MinorVersion = ?
			);",
		)?;
		let rows = statement.query_map(
			[version.major, version.minor, version.major, version.minor],
			|row| row.get(0),
		)?;

		let mut setting_ids = Vec::<u32>::new();
		for row in rows {
			setting_ids.push(row?);
		}
		Ok(setting_ids)
	}

	fn get_ascii_settings(
		conn: &mut Connection,
		versions: &[Version],
	) -> anyhow::Result<AsciiSettings> {
		log::info!("loading ASCII setting data ...");

		// Get the names of settings associated with a peripheral, meaning they are axis scope.
		let mut axis_scope_setting_names = HashSet::<String>::default();
		{
			let mut statement = conn.prepare(
				"SELECT DISTINCT DS.ASCIIName
				FROM Data_ProductsSettings AS DPS
				JOIN Data_Products AS DP ON DP.Id = DPS.ProductId
				JOIN Data_Settings AS DS ON DS.Id = DPS.SettingId
				WHERE DP.ParentId IS NOT NULL
				AND ASCIIName IS NOT NULL;",
			)?;
			let rows = statement.query_and_then([], |row| -> rusqlite::Result<_> { row.get(0) })?;

			for row in rows {
				let name = row?;
				axis_scope_setting_names.insert(name);
			}
		}

		// Collect the actual setting information
		let mut setting_info = FnvHashMap::default();
		{
			let statement = "SELECT
					Id,
					ParamType,
					EnumType,
					ASCIIName
				FROM Data_Settings
				WHERE ASCIIName IS NOT NULL;";
			let mut statement = conn.prepare(statement)?;
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
			for row in rows {
				let (id, info) = row?;
				setting_info.insert(id, info);
			}
		}

		// Get version information for each setting
		let mut settings = BTreeMap::<String, _>::default();
		for version in versions {
			log::info!("loading {version} data...");
			let setting_ids = Self::get_related_setting_ids(conn, *version)?;

			for id in setting_ids {
				let Some((variant, name)) = setting_info.get(&id) else {
					// This isn't an ASCII setting
					continue;
				};
				let setting_entry = settings.entry(name.clone()).or_insert_with(|| {
					(
						if axis_scope_setting_names.contains(name) {
							Scope::Axis
						} else {
							Scope::Device
						},
						FnvHashMap::default(),
					)
				});
				let versions: &mut BTreeSet<Version> = setting_entry.1.entry(*variant).or_default();
				versions.insert(*version);
			}
		}

		// Check that each setting only has one variant per version.
		let mut passed = true;
		let mut found_versions = FnvHashSet::default();
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

	/// Get a mapping of all the parameter types, keyed by parameter ID.
	fn get_param_types(conn: &mut Connection) -> anyhow::Result<FnvHashMap<u32, ParamType>> {
		let mut statement = conn.prepare("SELECT Id, Name FROM Data_ParamTypes;")?;
		let rows = statement.query_and_then([], |row| -> rusqlite::Result<_> {
			Ok((row.get(0)?, ParamType { name: row.get(1)? }))
		})?;
		let mut types = FnvHashMap::default();
		for row in rows {
			let (id, param_type) = row?;
			types.insert(id, param_type);
		}
		Ok(types)
	}

	/// Get a mapping of enums, keyed by enum ID.
	fn get_enums(conn: &mut Connection) -> anyhow::Result<FnvHashMap<u32, Enum>> {
		let mut statement = conn.prepare(
			"SELECT
				Type.Id,
				Type.Name,
				Type.Description,
				Var.Name,
				Var.Description
			FROM Data_EnumTypes AS Type
			JOIN Data_EnumNodes AS Var ON Var.TypeId = Type.Id;",
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
		let mut enums = FnvHashMap::default();
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
	pub fn rust_setting_value_type_name(&self, variant: &AsciiVariant) -> String {
		let param_type = self
			.param_types
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
