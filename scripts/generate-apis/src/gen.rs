//! Functions for generating/updated zproto source files.

use crate::{db::Data, Version, GENERATED_CONTENT_WARNING};
use crate::{protocol_manual_link, setting_rust_type_name, AsciiVariant, Scope};
use anyhow::Context as _;
use std::fs::File;
use std::io::Write;
use std::path::Path;

/// Generate the Rust source files for each version of firmware.
pub fn generate_ascii_setting_sources(
	data: &Data,
	dir_path: impl AsRef<Path>,
) -> anyhow::Result<()> {
	let dir_path = dir_path.as_ref();

	for version in &data.versions {
		let src_version = version.source_display();

		// Write the data into specified file as Rust source code.
		let out_path = dir_path.join(format!("v{src_version}.rs"));
		let mut file = File::create(&out_path)
			.with_context(|| format!("could not create {}", out_path.display()))?;
		log::info!("generating {}", out_path.display());

		writeln!(
			&mut file,
			r#"//! Settings in firmware version {version}.

{GENERATED_CONTENT_WARNING}

use crate::ascii::setting::Setting;
use crate::ascii::chain::scope::{{AxisScope, DeviceScope}};

define_settings! {{
"#
		)?;

		// Define each setting in the module
		let mut settings = Vec::with_capacity(data.ascii_settings.len());
		for (name, (scope, variants)) in &data.ascii_settings {
			for (variant, versions) in variants {
				if versions.contains(version) {
					settings.push((name, *scope, variant));
					write_setting_def(&mut file, name, *scope, variant, *version, data)?;
				}
			}
		}

		writeln!(&mut file, "}}")?;

		// Define an enum that can be any setting in the version
		write_any_setting_def(&mut file, *version, &settings)?;
	}
	Ok(())
}

/// Write the definition for a Rust enum representing any setting available the provided settings.
fn write_any_setting_def<W: Write>(
	f: &mut W,
	version: Version,
	settings: &[(&String, Scope, &AsciiVariant)],
) -> anyhow::Result<()> {
	// Define AnySetting
	writeln!(
		f,
		r#"define_any_setting! {{
/// Any setting available in firmware version {version}.
pub enum AnySetting {{"#
	)?;
	for (name, _scope, _variant) in settings {
		writeln!(
			f,
			"    /// The [{name}]({link}) setting.\n    {type_name},",
			name = name,
			link = protocol_manual_link(name, version),
			type_name = setting_rust_type_name(name),
		)?;
	}
	writeln!(f, "}}\n}}")?;
	Ok(())
}

/// Write the definition for a Rust type representing the specified setting.
fn write_setting_def<W: Write>(
	f: &mut W,
	name: &str,
	scope: Scope,
	variant: &AsciiVariant,
	version: Version,
	data: &Data,
) -> anyhow::Result<()> {
	let type_name = setting_rust_type_name(name);
	let value_type = data.rust_setting_value_type_name(variant);
	let link = protocol_manual_link(name, version);
	let scope_trait_name = scope.trait_name();
	writeln!(
		f,
		r#"    /// The type of the [`{name}`]({link}) setting.
    pub struct {type_name}: Setting<Type = {value_type}, Name = "{name}">, {scope_trait_name};"#
	)?;
	Ok(())
}

/// Write `mod.inc`, which contains all `pub mod` statements for all the
/// version specific modules we will create. This file should be
/// `include!(..)`ed into the parent module file.
pub fn generate_mod_inc(
	versions: &[Version],
	latest_version: Version,
	dir_path: impl AsRef<Path>,
) -> anyhow::Result<()> {
	let out_path = dir_path.as_ref().join("mod.inc");
	let mut file = File::create(&out_path)
		.with_context(|| format!("could not create {}", out_path.display()))?;
	let file = &mut file;

	writeln!(file, "{GENERATED_CONTENT_WARNING}\n")?;
	writeln!(file, "pub(crate) mod private {{")?;
	for version in versions {
		if *version == latest_version {
			writeln!(
				file,
				"\t#[cfg(any(feature = \"v{}\", feature = \"v_latest\"))]",
				version.source_display()
			)?;
		} else {
			writeln!(
				file,
				"\t#[cfg(feature = \"v{}\")]",
				version.source_display()
			)?;
		}
		writeln!(file, "\tpub mod v{};", version.source_display())?;
	}
	writeln!(file, "}}")?;

	for version in versions {
		writeln!(
			file,
			r#"#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "v{0}")))]
#[cfg(feature = "v{0}")]
pub use private::v{0} as v{0};"#,
			version.source_display()
		)?;
	}
	// Define v_latest module. Add an empty line in the documentation so that
	// the docs for the module it aliases are concatenated as a new paragraph.
	writeln!(
		file,
		r#"
/// An alias for the module containing settings for the latest version of firmware.
///
/// This alias is updated as new firmware versions are released and is excluded from
/// all semver guarantees (i.e. changing it does not necessitate a major version bump).
///
#[cfg_attr(all(doc, feature = "doc_cfg"), doc(cfg(feature = "v_latest")))]
#[cfg(feature = "v_latest")]
pub use private::v{} as v_latest;"#,
		latest_version.source_display()
	)?;
	Ok(())
}

/// Update the zproto Cargo.toml.
///
/// This adds features for all versions that it generated.
pub fn update_cargo_toml(versions: &[Version], dir_path: impl AsRef<Path>) -> anyhow::Result<()> {
	use std::io::{Read as _, Write as _};
	use toml_edit::{value, Array, Document, Item};

	let dir_path = dir_path.as_ref();

	let mut contents = String::new();
	std::fs::File::open(dir_path)?.read_to_string(&mut contents)?;
	let mut toml: Document = contents.parse()?;
	let Some(Item::Table(features)) = toml.get_mut("features") else {
		anyhow::bail!("unexpected Cargo.toml structure");
	};
	for version in versions {
		features
			.entry(&format!("v{}", version.source_display()))
			.or_insert(value(Array::default()))
			.as_value_mut()
			.unwrap()
			.as_array_mut()
			.unwrap()
			.decor_mut()
			.set_suffix(format!(
				" # Enable APIs related to firmware version {version}"
			));
	}
	features
		.entry("v_latest")
		.or_insert(value(Array::default()))
		.as_value_mut()
		.unwrap()
		.as_array_mut()
		.unwrap()
		.decor_mut()
		.set_suffix(" # Enable APIs related to the latest firmware version");

	features.sort_values_by(|key_a, _, key_b, _| {
		use std::cmp::Ordering;
		match (key_a.get(), key_b.get()) {
			("default", _) => Ordering::Less,
			(_, "default") => Ordering::Greater,
			("doc_cfg", _) => Ordering::Greater,
			(_, "doc_cfg") => Ordering::Less,
			(a, b) => a.cmp(b),
		}
	});

	let mut file = std::fs::File::options()
		.write(true)
		.truncate(true)
		.open(dir_path)?;
	write!(&mut file, "{toml}")?;

	Ok(())
}
