//! Tests to confirm that port methods accept/reject commands appropriately at
//! compile time.
use zproto::{
    backend::Backend,
    binary::{command::*, Port}
};

fn _fail<B: Backend>(port: &mut Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    let _ = port.tx_recv((0, RESET))?;  // Shouldn't work with tx_recv
    let _ = port.tx_recv((0, HOME, 0i32))?;  // Shouldn't take parameters
    let _ = port.tx_recv((0, MOVE_ABSOLUTE))?;  // Should take parameters
    let _ = port.tx_recv((0, MOVE_ABSOLUTE, false))?;  // Wrong parameter type

    let _ = port.tx_recv((0, untyped::HOME))?;  // u8 commands require an explicit data value

    port.tx((0, ERROR))?;  // Reply-only commands cannot be transmitted
    port.tx((0, ERROR, 0i32))?;  // Reply-only commands cannot be transmitted
    Ok(())
}

fn _ok<B: Backend>(port: &mut Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    port.tx((1, RESET))?;  // RESET can be transmitted
    let _ = port.recv(MANUAL_MOVE_TRACKING)?; // Reply-only commands can be received
    Ok(())
}

fn main() {}
