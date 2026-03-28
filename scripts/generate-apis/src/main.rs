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
		dir
	};
	std::fs::create_dir_all(&dir)?;

	log::info!("generating settings ...");
	gen::generate_ascii_setting_source(&data, dir.join("settings.inc"))?;

	log::info!("done");
	Ok(())
}
