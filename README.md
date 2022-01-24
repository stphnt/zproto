## zaber-protocol-rs

A library from communicating with Zaber products in Rust.

This library is unofficial. [Zaber Motion Library](https://www.zaber.com/software) is Zaber's official communications library, but as of this writing there are no Rust bindings.

## Usage

Add this to you `Cargo.toml:

```toml
[dependencies]
zaber-protocol = "0.1"
```

## Getting started

See the [`ascii`](https://docs.rs/zaber-protocol/latest/zaber-protocol/ascii) or [`binary`](https://docs.rs/zaber-protocol/latest/zaber-protocol/ascii) module documentation for an introduction to communicating with devices using the Zaber ASCII or Binary protocols.

The [examples](examples) folder has some very simple applications. To run an example:

* clone this repository
* inside the repository, run:
```sh
cargo run --example <example-file-name-without-extension>
```

## Documentation

The documentation is available on [docs.rs](https://docs.rs/zaber-serial-rs).

## License

This project is license under the [MIT License](LICENSE).

## Troubleshooting

1. Building fails with an error about `libudev`

    This crate requires the `libudev` shared library to be installed on your system, which some operating systems do not have installed by default.
    Installing the library will solve the problem.
    For example, on Ubuntu the library can be installed with `sudo apt install libudev-dev`.
