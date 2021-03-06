use simple_logger::SimpleLogger;
use zproto::ascii::{check, Port, Warning};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Enable logging
    SimpleLogger::new().init().unwrap();

    // Open the port, home device 1, and wait for it to finish.
    let mut port = Port::open_serial("/dev/ttyUSB0")?;
    port.command_reply_with_check(
        (1, "home"),
        check::warning_in(("WR", "WH", Warning::NONE)), // Ignore warnings about it being unhomed.
    )?;
    port.poll_until_idle(1)?;

    // Move towards the end of travel and monitor position as it goes.
    // Once the position exceeds a specific limit, interrupt the motion.
    port.command_reply((1, 1, "move max"))?;
    port.poll_until((1, 1, "get pos"), |reply| {
        let pos: i32 = reply.data().parse().unwrap();
        pos >= 100000
    })?;
    port.command_reply_with_check((1, "stop"), check::warning_is("NI"))?;
    Ok(())
}
