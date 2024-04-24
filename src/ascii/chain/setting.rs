//! Types for accessing device or axis settings.
//!
//! The primary type of interest is the [`Settings`] type, which can be used to
//! generate [`Routine`]s for getting or setting device/axis settings on a [`Chain`]
//!
//! ## Example
//!
//! ```
//! # use zproto::error::AsciiError;
//! # fn wrapper() -> Result<(), AsciiError> {
//! use zproto::ascii::{Port, setting::v_latest::{DeviceId, Version}};
//!
//! let mut port = Port::open_serial("/dev/ttyUSB0")?;
//! let chain = port.chain()?;
//! for device in &chain {
//!     let settings = device.settings();
//!     let device_id = port.run(settings.get(DeviceId))?;
//!     let version = port.run(settings.get(Version))?;
//!     println!("{device_id} {version}");
//! }
//! # Ok(())
//! # }
//! ```
//!
//! [`Chain`]: crate::ascii::chain::Chain

use crate::{
	ascii::{
		chain::{
			data_type::DataType,
			info::ChainInfo,
			scope::{RequiresAxisScope, RequiresDeviceScope, SatisfiesRequiredScope},
			Axis, Device,
		},
		command::Target,
		response::{
			check::{self, Check},
			Reply,
		},
		setting::Setting,
		Port,
	},
	backend::Backend,
	error::AsciiError,
	routine::Routine,
};

/// An interface for getting and/or setting the settings on a single device or axis.
///
/// A [`Settings`] instance is created via the [`Device::settings`](super::Device::settings)
/// and [`Axis::settings`](super::Axis::settings) methods.
#[derive(Debug)]
pub struct Settings<'a, S, Tag> {
	_info: &'a ChainInfo,
	target: Target,
	_scope_marker: std::marker::PhantomData<S>,
	_tag: std::marker::PhantomData<Tag>,
}

/// A type of [`Settings`] that gives access to axis-scope settings.
///
/// For more details see [`Settings`].
pub type AxisSettings<'a, Tag> = Settings<'a, RequiresAxisScope, Tag>;

impl<'a, Tag> AxisSettings<'a, Tag> {
	pub(crate) fn new_axis(axis: &Axis<'a, Tag>) -> Self {
		Settings {
			_info: axis.info,
			target: axis.target(),
			_scope_marker: std::marker::PhantomData,
			_tag: std::marker::PhantomData,
		}
	}
}

/// A type of [`Settings`] that gives access to device-scope settings.
///
/// For more details see [`Settings`].
pub type DeviceSettings<'a, Tag> = Settings<'a, RequiresDeviceScope, Tag>;

impl<'a, Tag> DeviceSettings<'a, Tag> {
	pub(crate) fn new_device(device: &Device<'a, Tag>) -> Self {
		Settings {
			_info: device.info,
			target: device.target(),
			_scope_marker: std::marker::PhantomData,
			_tag: std::marker::PhantomData,
		}
	}
}

impl<'a, S, Tag> Settings<'a, S, Tag> {
	/// Get the value of a setting.
	///
	/// The reply's warning flag and status fields are not checked. The reply is expected to be "OK".
	pub fn get<T>(&self, setting: T) -> Get<T, Tag>
	where
		T: Setting + SatisfiesRequiredScope<S>,
	{
		Get {
			target: self.target,
			setting,
			tag: std::marker::PhantomData,
		}
	}

	/// Same as [`Settings::get`] except that the reply is validated with the custom [`Check`].
	pub fn get_with_check<T, C>(&self, setting: T, checker: C) -> GetWithCheck<T, C, Tag>
	where
		T: Setting + SatisfiesRequiredScope<S>,
		C: check::Check<Reply>,
	{
		GetWithCheck {
			target: self.target,
			setting,
			checker,
			tag: std::marker::PhantomData,
		}
	}

	/// Set the value of a setting.
	///
	/// The reply's warning flag and status fields are not checked. The reply is expected to be "OK".
	pub fn set<T, V>(&self, setting: T, value: V) -> Set<T, V, Tag>
	where
		T: Setting + SatisfiesRequiredScope<S>,
		V: std::borrow::Borrow<<T::Type as DataType>::Borrowed>,
	{
		Set {
			target: self.target,
			setting,
			value,
			tag: std::marker::PhantomData,
		}
	}

	/// Same as [`Settings::set`] except that the reply is validated with the custom [`Check`].
	pub fn set_with_check<T, V, C>(
		&self,
		setting: T,
		value: V,
		checker: C,
	) -> SetWithCheck<T, V, C, Tag>
	where
		T: Setting + SatisfiesRequiredScope<S>,
		V: std::borrow::Borrow<<T::Type as DataType>::Borrowed>,
		C: check::Check<Reply>,
	{
		SetWithCheck {
			target: self.target,
			setting,
			value,
			checker,
			tag: std::marker::PhantomData,
		}
	}
}

/// A routine that will get the value of a device or axis setting, `T`.
///
/// It is created via the [`get`] method on [`Settings`].
///
/// [`get`]: Settings::get
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct Get<T, Tag> {
	target: Target,
	setting: T,
	tag: std::marker::PhantomData<Tag>,
}

impl<'a, B, T, Tag> Routine<Port<'a, B, Tag>> for Get<T, Tag>
where
	B: Backend,
	T: Setting,
	<T::Type as DataType>::Owned: 'a,
{
	type Output = <T::Type as DataType>::Owned;
	type Error = AsciiError;

	fn run(&mut self, port: &mut Port<'a, B, Tag>) -> Result<Self::Output, Self::Error> {
		let reply = port
			.command_reply((self.target, format!("get {}", self.setting.name())))?
			.flag_ok()?;
		Ok(T::Type::parse(reply.data())?)
	}
}

/// A routine that will get the value of a device or axis setting, `T`.
/// Responses are validated with the custom [`Check`], `C`.
///
/// It is created via the [`get_with_check`] method on [`Settings`].
///
/// [`get_with_check`]: Settings::get_with_check
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct GetWithCheck<T, C, Tag> {
	target: Target,
	setting: T,
	checker: C,
	tag: std::marker::PhantomData<Tag>,
}

impl<'a, B, T, C, Tag> Routine<Port<'a, B, Tag>> for GetWithCheck<T, C, Tag>
where
	B: Backend,
	T: Setting,
	for<'b> &'b C: Check<Reply>,
	<T::Type as DataType>::Owned: 'a,
{
	type Output = <T::Type as DataType>::Owned;
	type Error = AsciiError;

	fn run(&mut self, port: &mut Port<'_, B, Tag>) -> Result<Self::Output, Self::Error> {
		let reply = port
			.command_reply((self.target, format!("get {}", self.setting.name())))?
			.check(&self.checker)?;
		Ok(T::Type::parse(reply.data())?)
	}
}

/// A routine that will set a device or axis setting, `T`, to a value with type `V`.
///
/// It is created via the [`set`] method on [`Settings`].
///
/// [`set`]: Settings::set
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct Set<T, V, Tag> {
	target: Target,
	setting: T,
	value: V,
	tag: std::marker::PhantomData<Tag>,
}
impl<'a, B, T, V, Tag> Routine<Port<'a, B, Tag>> for Set<T, V, Tag>
where
	B: Backend,
	T: Setting,
	V: std::borrow::Borrow<<T::Type as DataType>::Borrowed>,
{
	type Output = ();
	type Error = AsciiError;

	fn run(&mut self, port: &mut Port<'a, B, Tag>) -> Result<Self::Output, Self::Error> {
		let _ = port
			.command_reply((
				self.target,
				format!(
					"set {} {}",
					self.setting.name(),
					T::Type::display(self.value.borrow())
				),
			))?
			.flag_ok_and(check::minimal())?;
		Ok(())
	}
}

/// A routine that will set a device or axis setting, `T`, to a value with type `V`.
/// Responses are validated with the custom [`Check`], `C`.
///
/// It is created via the [`set_with_check`] method on [`Settings`].
///
/// [`set_with_check`]: Settings::set_with_check
#[derive(Debug)]
#[must_use = "routines are lazy and do nothing unless consumed"]
pub struct SetWithCheck<T, V, C, Tag> {
	target: Target,
	setting: T,
	value: V,
	checker: C,
	tag: std::marker::PhantomData<Tag>,
}
impl<'a, B, T, V, C, Tag> Routine<Port<'a, B, Tag>> for SetWithCheck<T, V, C, Tag>
where
	B: Backend,
	T: Setting,
	V: std::borrow::Borrow<<T::Type as DataType>::Borrowed> + 'a,
	for<'b> &'b C: check::Check<Reply>,
{
	type Output = ();
	type Error = AsciiError;

	fn run(&mut self, port: &mut Port<'a, B, Tag>) -> Result<Self::Output, Self::Error> {
		let _ = port
			.command_reply((
				self.target,
				format!(
					"set {} {}",
					self.setting.name(),
					T::Type::display(self.value.borrow())
				),
			))?
			.check(&self.checker)?;
		Ok(())
	}
}

#[cfg(test)]
mod test {
	use super::super::test::{new_mock_chain, new_mock_port_and_chain};
	use super::*;
	use crate::ascii::chain::scope::{AxisScope, DeviceScope};

	struct DeviceSetting;
	impl Setting for DeviceSetting {
		type Type = u8;
		fn name(&self) -> &str {
			"device.scope.setting"
		}
	}
	impl DeviceScope for DeviceSetting {}

	struct AxisSetting;
	impl Setting for AxisSetting {
		type Type = u32;
		fn name(&self) -> &str {
			"axis.scope.setting"
		}
	}
	impl AxisScope for AxisSetting {}

	/// Ensure settings of the appropriate scope are accepted by the `get()` and
	/// `set()` methods.
	#[test]
	fn setting_scope_restrictions() {
		let (mut port, chain) = new_mock_port_and_chain(1, 1);

		// `get()` accepts appropriately scoped settings and parse the values as the
		// appropriate type
		let device = chain.device(1).unwrap();
		let device_settings = device.settings();
		{
			port.backend_mut().append_data(b"@01 0 OK IDLE FZ 23\r\n");
		}
		let value = device_settings.get(DeviceSetting).run(&mut port).unwrap();
		assert_eq!(value, 23u8);

		let axis_settings = device.axis(1).unwrap().settings();
		{
			port.backend_mut().append_data(b"@01 1 OK IDLE FZ 34\r\n");
		}
		let value = axis_settings.get(AxisSetting).run(&mut port).unwrap();
		assert_eq!(value, 34u32);

		// `set()` accepts values of the appropriate type.
		let _ = device_settings.set(DeviceSetting, 1u8).run(&mut port);
		let _ = axis_settings.set(AxisSetting, 1u32).run(&mut port);
	}

	/// Ensure `set()` accepts the appropriate types
	#[test]
	fn set_accepted_values() {
		let chain = new_mock_chain(1, 1);
		let settings = chain.device(1).unwrap().settings();
		// The value's type should be appropriately inferred.
		let _ = settings.set(DeviceSetting, 1);
		// Explicit type (u8) should work.
		let _ = settings.set(DeviceSetting, 1u8);
		// Reference to the appropriate type, whether inferred or not, should work.
		let _ = settings.set(DeviceSetting, &1);
		let _ = settings.set(DeviceSetting, &1u8);
		// Both &str and String should be accepted for settings with data type String.
		let _ = settings.set("something", "foo");
		let _ = settings.set("something", "foo".to_string());
	}

	/// Ensure that Strings are accepted as valid axis- and device-scope settings.
	#[test]
	fn strings_as_settings() {
		let (mut port, chain) = new_mock_port_and_chain(1, 1);
		let port = &mut port;
		{
			let settings = chain.device(1).unwrap().settings();
			let _: Result<String, AsciiError> = settings.get("anything").run(port);
			let _: Result<String, AsciiError> = settings.get("anything".to_string()).run(port);
			let _: Result<String, AsciiError> = settings.get(&"anything".to_string()).run(port);
		}
		{
			let settings = chain.device(1).unwrap().axis(1).unwrap().settings();
			let _: Result<String, AsciiError> = settings.get("anything").run(port);
			let _: Result<String, AsciiError> = settings.get("anything".to_string()).run(port);
			let _: Result<String, AsciiError> = settings.get(&"anything".to_string()).run(port);
		}
	}
}
