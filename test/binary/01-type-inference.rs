use zproto::binary::{DeviceMessage, Port, command::*};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut port = Port::open_serial("/dev/nonexistant")?;
    // Confirm the expected return types by explicitly marking them an INCORRECT type
    let _: DeviceMessage<types::Reset> = port.tx_recv((0, HOME))?;
    let _: DeviceMessage<types::Home> = port.tx_recv((0, untyped::HOME, 0i32))?;
    let _: DeviceMessage<types::ReturnSetting> = port.tx_recv((0, RETURN_SETTING, RETURN_CURRENT_POSITION))?;
    let _: DeviceMessage<types::ReturnSetting> = port.tx_recv((0, RETURN_SETTING, SET_TARGET_SPEED))?;
    Ok(())
}