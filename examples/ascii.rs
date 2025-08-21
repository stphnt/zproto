//! Example script for using Zaber's ASCII protocol via zproto.

use simple_logger::SimpleLogger;
use zproto::ascii::{
	response::{check, Warning},
	Port,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	// Enable logging
	SimpleLogger::new().init().unwrap();

	// Open the port, home device 1, and wait for it to finish.
	let mut port = Port::open_serial("/dev/ttyUSB0")?;
	port.command_reply((1, "home"))?.flag_ok_and(
		check::warning_in(("WR", "WH", Warning::NONE)), // Ignore warnings about it being unhomed.
	)?;
	port.poll_until_idle(1, check::flag_ok())?;

	// Move towards the end of travel and monitor position as it goes.
	// Once the position exceeds a specific limit, interrupt the motion.
	port.command_reply((1, 1, "move max"))?
		.flag_ok_and(check::warning_is_none())?;
	port.poll_until((1, 1, "get pos"), check::flag_ok(), |reply| {
		let pos: i32 = reply.data().parse().unwrap();
		pos >= 100_000
	})?;
	port.command_reply((1, "stop"))?
		.flag_ok_and(check::warning_is("NI"))?;
	Ok(())
}
