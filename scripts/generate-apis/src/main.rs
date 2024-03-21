use argh::FromArgs;
use generate_apis::{db, gen};
use std::path::PathBuf;

/// Generate rust source code from a Zaber device database.
#[derive(Debug, FromArgs)]
struct App {
	/// the path to the database
	#[argh(positional)]
	database: PathBuf,
	/// the root of the zproto repository
	#[argh(positional)]
	dir: PathBuf,
}

fn main() -> anyhow::Result<()> {
	env_logger::init();

	let app: App = argh::from_env();

	// Collect setting information for each supported version in the database.
	log::info!("loading {} ...", app.database.display());
	let data = {
		let mut conn = db::get_connection(&app.database)?;
		db::Data::new(&mut conn)?
	};

	let dir = {
		let mut dir = app.dir.clone();
		dir.push("src");
		dir.push("ascii");
		dir.push("setting");
		dir
	};
	std::fs::create_dir_all(&dir)?;
	gen::generate_mod_inc(&data.versions, *data.versions.iter().last().unwrap(), &dir)?;

	log::info!("generating settings ...");
	let src_dir = dir.join("private");
	std::fs::create_dir_all(&src_dir)?;
	gen::generate_ascii_setting_sources(&data, src_dir)?;

	log::info!("updating Cargo.toml ...");
	gen::update_cargo_toml(&data.versions, app.dir.join("Cargo.toml"))?;
	log::info!("done");
	Ok(())
}
