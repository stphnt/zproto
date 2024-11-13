# Changelog

## [Unreleased]

## [0.4.0] - 2024-11-12

### Added

* The `backend::Mock` has been added for mocking communication with a Zaber product in unit tests. Create one via `Port::open_mock()`. It is only available if the `mock` feature is enabled, which is disabled by default.

### Changed

* `ascii::Port` methods now mostly return `NotChecked<R>` instead of some response type `R`. The `NotChecked<R>` type wraps a response `R`, which is only accessible by calling one of the type's validation functions.
* `ascii::Port` is now parameterized with a `Tag` type, which has a default value. In the vase majority of cases it can be ignored. See the `ascii::Port` documentation for more details.
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


[Unreleased]: https://github.com/stphnt/zproto/compare/v0.4.0...HEAD
[0.4.0]: https://github.com/stphnt/zproto/compare/v0.3.5...v0.4.0
