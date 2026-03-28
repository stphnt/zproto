//! Functions for generating/updated zproto source files.

use crate::{db::Data, Version, GENERATED_CONTENT_WARNING};
use crate::{protocol_manual_link, setting_rust_type_name, AsciiVariant, Scope};
use anyhow::Context as _;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::Write;
use std::ops::RangeInclusive;
use std::path::Path;

/// Generate the source file for ASCII settings.
pub fn generate_ascii_setting_source(data: &Data, path: impl AsRef<Path>) -> anyhow::Result<()> {
	let path = path.as_ref();
	let mut file =
		File::create(path).with_context(|| format!("could not create {}", path.display()))?;
	log::info!("generating {}", path.display());

	writeln!(
		&mut file,
		"{GENERATED_CONTENT_WARNING}

define_settings! {{
"
	)?;

	let version_range = *data.versions.iter().min().unwrap()..=*data.versions.iter().max().unwrap();
	for (name, (scope, variants)) in &data.ascii_settings {
		write_setting_def(
			&mut file,
			name,
			*scope,
			variants,
			version_range.clone(),
			data,
		)?;
	}

	writeln!(&mut file, "}}")?;

	Ok(())
}

/// Write the definition for a Rust type representing the specified setting.
fn write_setting_def<W: Write>(
	f: &mut W,
	name: &str,
	_scope: Scope,
	variant_data: &BTreeMap<AsciiVariant, BTreeSet<Version>>,
	version_range: RangeInclusive<Version>,
	data: &Data,
) -> anyhow::Result<()> {
	let setting_version_range = variant_data.values().fold(
		Version {
			major: 100,
			minor: 0,
		}..=Version { major: 0, minor: 0 },
		|range, versions| {
			*range.start().min(versions.first().unwrap())
				..=*range.end().max(versions.last().unwrap())
		},
	);
	let type_name = setting_rust_type_name(name);
	let mut variant_value_types: Vec<_> = variant_data
		.keys()
		.map(|variant| data.rust_setting_value_type_name(name, variant))
		.collect();
	variant_value_types.sort();
	variant_value_types.dedup();
	let value_type_list = variant_value_types.join(", ");
	let link = protocol_manual_link(name, *setting_version_range.end());

	writeln!(
		f,
		"/// The type representing the [`{name}`]({link}) setting."
	)?;

	let (start, end) = (setting_version_range.start(), setting_version_range.end());
	if start > version_range.start() {
		writeln!(f, "///\n/// Introduced in firmware version {start}.",)?;
	}
	// The FW7 version numbers are not contiguous, so this check only works for FW6.
	if end.major == 6 && end < version_range.end() {
		writeln!(f, "///\n/// Removed after firmware version {end}.")?;
	}
	writeln!(
		f,
		r#""{name}" => pub struct {type_name}: {value_type_list}"#
	)?;
	Ok(())
}
