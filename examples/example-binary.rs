use simple_logger::SimpleLogger;
use zaber_protocol::ascii::{check::unchecked, IntoCommand as _, Port as AsciiPort};
use zaber_protocol::binary::{command::*, Port};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Enable logging
    SimpleLogger::new().init().unwrap();

    let port_path = "/dev/ttyUSB0";
    let device = 1;
    {
        // If necessary, convert to binary from ASCII.
        let mut port = AsciiPort::open_serial(&port_path)?;
        let mut guard = port.timeout_guard(Some(std::time::Duration::from_secs(1)))?;
        let _ = guard.command_reply_with_check("tools setcomm 9600 1".to_all(), unchecked());
    }

    // Open the port and set up the chain
    let mut port = Port::open_serial(&port_path)?;
    port.tx_rx_until_timeout((0, RENUMBER, 0))?;
    port.message_ids(true)?;

    // Home device 1
    port.tx_rx((device, HOME))?;
    port.poll_until_idle(device)?;

    // Move towards the end of travel and monitor position as it goes.
    port.tx_rx((device, MOVE_ABSOLUTE, 100_000))?;
    port.poll_until((device, RETURN_CURRENT_POSITION), |reply| {
        let pos = reply.data().unwrap();
        println!("{}", pos);
        pos == 100_000
    })?;

    port.tx_rx((device, MOVE_ABSOLUTE, 25_000))?;
    port.poll_until_idle(1)?;
    let reply = port.tx_rx((device, RETURN_CURRENT_POSITION))?;
    println!("{}", reply.data()?);
    Ok(())
}
