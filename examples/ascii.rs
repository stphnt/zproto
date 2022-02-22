use simple_logger::SimpleLogger;
use zproto::ascii::{check, IntoCommand as _, Port, Warning};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Enable logging
    SimpleLogger::new().init().unwrap();

    // Open the port, home device 1, and wait for it to finish.
    let mut port = Port::open_serial("/dev/ttyUSB0")?;
    port.command_reply_with_check(
        "home".target(1),
        check::warning_in(("WR", "WH", Warning::NONE)), // Ignore warnings about it being unhomed.
    )?;
    port.poll_until_idle(1)?;

    // Move towards the end of travel and monitor position as it goes.
    // Once the position exceeds a specific limit, interrupt the motion.
    port.command_reply("move max".target((1, 1)))?;
    port.poll_until("get pos".target((1, 1)), |reply| {
        let pos: i32 = reply.data().parse().unwrap();
        pos >= 100000
    })?;
    port.command_reply_with_check("stop".target(1), check::warning_is("NI"))?;
    Ok(())
}
