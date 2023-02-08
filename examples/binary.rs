use simple_logger::SimpleLogger;
use zproto::binary::{command::*, Port};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Enable logging
    SimpleLogger::new().init().unwrap();

    let port_path = "/dev/ttyUSB0";
    let device = 1;

    // Open the port and set up the chain
    let mut port = Port::open_serial(&port_path)?;
    port.tx_recv_until_timeout((0, RENUMBER, 0))?;
    port.set_message_ids(true)?;

    // Home device 1
    port.tx_recv((device, HOME))?;
    port.poll_until_idle(device)?;

    // Move towards the end of travel and monitor position as it goes.
    port.tx_recv((device, MOVE_ABSOLUTE, 100_000))?;
    port.poll_until((device, RETURN_CURRENT_POSITION), |reply| {
        let pos = reply.data().unwrap();
        println!("{pos}");
        pos == 100_000
    })?;

    port.tx_recv((device, MOVE_ABSOLUTE, 25_000))?;
    port.poll_until_idle(1)?;
    let reply = port.tx_recv((device, RETURN_CURRENT_POSITION))?;
    println!("{}", reply.data()?);
    Ok(())
}
