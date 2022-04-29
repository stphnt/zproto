use zproto::{
    backend::Backend,
    binary::{command::*, Port}
};

fn _fail<B: Backend>(port: &mut Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    let reply = port.tx_recv((0, MOVE_ABSOLUTE, 1000))?;
    let value: bool = reply.data()?;  // Wrong data type, should be i32
    Ok(())
}

fn _ok<B: Backend>(port: &mut Port<B>) -> Result<(), Box<dyn std::error::Error>> {
    let reply = port.tx_recv((0, MOVE_ABSOLUTE, 1000))?;
    let value = reply.data()?;  // Shouldn't need any type hints
    let value: bool = reply.to_untyped().data()?; // Although incorrect, this is not a compile-time error.
    Ok(())
}

fn main() {}
