# zproto

A Rust implementation of Zaber's ASCII and Binary Protocols.

This library is unofficial. [Zaber Motion Library](https://www.zaber.com/software)
is Zaber's official communications library, but as of this writing there are no Rust bindings.

## Usage

Add this to your `Cargo.toml:

```toml
[dependencies]
zproto = "0.2.0"
```

## Getting started

This library aims to be simple but robust. Communicating with a product in Zaber's
ASCII protocol looks something like this:

```rust,no_run
use zproto::{
    ascii::{Port, check, Warning},
    error::Error,
};

fn main() -> Result<(), Error> {
    // Open the port, home device 1, and wait for it to finish.
    let mut port = Port::open_serial("/dev/ttyUSB0")?;
    port.command_reply_with_check(
        (1, "home"),
        // Ignore warnings about it being unhomed.
        check::warning_in(("WR", "WH", Warning::NONE)),
    )?;
    port.poll_until_idle(1)?;

    // Move towards the end of travel and monitor position as it goes.
    // Once the position exceeds 100000, interrupt the motion.
    port.command_reply((1, "move max"))?;
    port.poll_until((1, "get pos"), |reply| {
        let pos: i32 = reply.data().parse().unwrap();
        pos >= 100_000
    })?;
    port.command_reply_with_check((1, "stop"), check::warning_is("NI"))?;
    Ok(())
}
```

See the [`ascii`](https://docs.rs/zproto/latest/zproto/ascii) or
[`binary`](https://docs.rs/zproto/latest/zproto/binary) module documentation for
a more in-depth introduction to communicating with devices using the Zaber ASCII
or Binary protocols.

The [examples](examples) folder has some simple applications.

## Documentation

The documentation is available on [docs.rs](https://docs.rs/zproto).

## Cargo Features

By default, both the ASCII and Binary protocols are enabled via the `ascii` and
`binary` Cargo features, respectively. However, if you only want to use one,
`ascii` for example, you can specify that in your `Cargo.toml`:

```toml
[dependencies]
zproto = { version = "0.2.0", default-features = false, features = ["ascii"] }
```

This will only include portions of the library related to the ASCII protocol
during compilation.

## License

This project is license under the [MIT License](LICENSE).

## Troubleshooting

1. Building fails with an error about `libudev`

    This crate requires the `libudev` shared library to be installed on your system, which some operating systems do not have installed by default.
    Installing the library will solve the problem.
    For example, on Ubuntu the library can be installed with `sudo apt install libudev-dev`.
