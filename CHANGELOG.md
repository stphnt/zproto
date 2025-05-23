# Changelog

## [Unreleased]

## [0.4.2] - 2025-03-17

### Changed

* `Open*Options::open_dyn()` methods now return a `Backend` that implements `Send`.
* `Port::try_into_send()` methods now require the `Port`'s `Backend` to implement `Send`.

## [0.4.1] - 2025-03-02

### Added

* Added `Port::open_general()` for opening a port using a general `Backend`.
* Added `ascii::Port::open_general_options()` and `ascii::Port::open_mock_options()`.

### Changed

* Unsealed the `Backend` trait, allowing for third-party implementations.
* Update `serialport-rs` dependency to 4.7.0
* `Mock::set_write_callback()` now takes an `impl FnMut` instead of an `fn`.

## [0.4.0] - 2024-11-12

### Added

* The `backend::Mock` has been added for mocking communication with a Zaber product in unit tests. Create one via `Port::open_mock()`. It is only available if the `mock` feature is enabled, which is disabled by default.
* `Port::open_serial_options()` and `Port::open_tcp_options()` convenience methods for creating `OptionSerialOptions` and `OpenTcpOptions`.

### Changed

* `ascii::Port` methods now mostly return `NotChecked<R>` instead of some response type `R`. The `NotChecked<R>` type wraps a response `R`, which is only accessible by calling one of the type's validation functions.
* `ascii::Port` is now parameterized with a `Tag` type, which has a default value. In the vast majority of cases it can be ignored. See the `ascii::Port` documentation for more details.
* The `ascii` module now only re-exports the `Port` type. All other types are now organized into submodules, some of which have been renamed.

  * `ascii::parse` -> `ascii::packet`
  * Moved to `ascii::command` module
    * `ascii::Command`
    * `ascii::Target`
    * `ascii::MaxPacketSize`
  * Moved to `ascii::port` module
    * `ascii::OpenSerialOptions`
    * `ascii::OpenTcpOptions`
    * `ascii::Port` (a re-export remains)
    * `ascii::Direction`
    * `ascii::SendPort`
  * Moved to `ascii::port::handlers` module
    * `ascii::LocalHandlers`
    * `ascii::SendHandlers`
    * `ascii::Handlers`
    * `ascii::PacketHandler`
    * `ascii::SendPacketHandler`
    * `ascii::SendUnexpectedAlertHandler`
    * `ascii::UnexpectedAlertHandler`
  * Moved to `ascii::response` module
    * `ascii::check` (module)
    * `ascii::Alert`
    * `ascii::Info`
    * `ascii::Reply`
    * `ascii::Warning`
    * `ascii::AnyResponse`
    * `ascii::Flag`
    * `ascii::Kind`
    * `ascii::Status`
    * `ascii::Response`
    * `ascii::ResponseWithFlag`
    * `ascii::ResponseWithStatus`
    * `ascii::ResponseWithWarning`
    * `ascii::SpecificResponse`
* The following items have been moved into the `binary::handlers` module
  * `binary::LocalHandlers`
  * `binary::SendHandlers`
  * `binary::Handlers`
  * `binary::PacketHandler`
  * `binary::SendPacketHandler`

### Removed

* The `ascii::PacketCallback` and `ascii::UnexpectedAlertCallback` have been removed.

## Prior releases

Prior release notes can be found in [GitHub Releases](https://github.com/stphnt/zproto/releases).


[Unreleased]: https://github.com/stphnt/zproto/compare/v0.4.2...HEAD
[0.4.2]: https://github.com/stphnt/zproto/compare/v0.4.1...v0.4.2
[0.4.1]: https://github.com/stphnt/zproto/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/stphnt/zproto/compare/v0.3.5...v0.4.0
