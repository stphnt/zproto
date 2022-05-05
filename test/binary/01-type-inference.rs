use zproto::binary::{Message, Port, command::*};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut port = Port::open_serial("/dev/nonexistant")?;
    // Confirm the expected return types by explicitly marking them an INCORRECT type
    let _: Message<types::Reset> = port.tx_recv((0, HOME))?;
    let _: Message<types::Home> = port.tx_recv((0, untyped::HOME, 0i32))?;
    let _: Message<types::ReturnSetting> = port.tx_recv((0, RETURN_SETTING, RETURN_CURRENT_POSITION))?;
    let _: Message<types::ReturnSetting> = port.tx_recv((0, RETURN_SETTING, SET_TARGET_SPEED))?;
    Ok(())
}
