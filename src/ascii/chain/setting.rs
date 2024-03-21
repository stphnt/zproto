//! Types and traits for accessing device or axis settings

use crate::{
	ascii::{
		chain::{info::ChainInfo, Axis, Device},
		data_type::DataType,
		marker::Markers,
		response::{check, Reply},
		scope::{AxisScope, AxisScopeMarker, DeviceScope, DeviceScopeMarker, SameScopeAsMarker},
		Port, Target,
	},
	backend::Backend,
	error::AsciiError,
	shared::SharedMut,
};

/// Define a type representing an ASCII setting.
macro_rules! define_settings {
    (
        $(
            $(#[$metadata:meta])*
            $visibility:vis struct $setting:ident : Setting<Type = $data_type:path, Name = $name:literal>, $scope:ident;
        )+
    ) => {
        $(
        $(#[$metadata])+
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        $visibility struct $setting;
        impl $setting {
            /// Returns the name of the setting.
            pub const fn name(&self) -> &'static str {
                $name
            }
        }
        impl crate::ascii::chain::setting::Setting for $setting {
            type Type = $data_type;
            fn name(&self) -> &str {
                self.name()
            }
        }
        impl $scope for $setting {}
        impl std::convert::AsRef<str> for $setting {
            /// Return the name of the setting.
            fn as_ref(&self) -> &str {
                self.name()
            }
        }
        impl std::str::FromStr for $setting {
            type Err = ();
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                if s == $setting.name() {
                    Ok($setting)
                } else {
                    Err(())
                }
            }
        }
        )+
    };
}

/// Define an enum that represents multiple ASCII settings. Each variant `$setting`
/// must also be the name for another type that implements [`Setting`].
///
/// This macro will automatically defined [`Setting`], the scope, and conversion
/// traits between it and the setting types it maps to.
macro_rules! define_any_setting {
    (
        $(#[$any_setting_meta:meta])*
        $visibility:vis enum $any_setting:ident {
            $(
                $(#[$setting_meta:meta])*
                $setting:ident
            ),+
            $(,)?
        }
    ) => {
        $(#[$any_setting_meta])*
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
        $visibility enum $any_setting {
            $(
                $(#[$setting_meta])*
                $setting
            ),+
        }
        impl crate::ascii::chain::setting::Setting for $any_setting {
            type Type = String;
            fn name(&self) -> &str {
                match self {
                    $(
                        Self::$setting => $setting.name()
                    ),+
                }
            }
        }
        impl crate::ascii::scope::AxisScope for $any_setting {}
        impl crate::ascii::scope::DeviceScope for $any_setting {}
        impl std::convert::AsRef<str> for $any_setting {
            fn as_ref(&self) -> &str {
                self.name()
            }
        }
        impl std::str::FromStr for $any_setting {
            type Err = ();
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Err(())
                $(
                    .or_else(|_| s.parse::<$setting>().map(From::from))
                )+
            }
        }
        $(
            impl std::convert::From<$setting> for $any_setting {
                fn from(_: $setting) -> Self {
                    Self::$setting
                }
            }
        )+
    };
}

include!("setting/mod.inc");

/// Any type that represents a setting
pub trait Setting {
	/// The setting's datatype.
	///
	/// This is used to convert the data to and from values in an ASCII protocol message.
	type Type: DataType;

	/// Get the name of the setting.
	fn name(&self) -> &str;
}

impl<T> Setting for &T
where
	T: Setting,
{
	type Type = T::Type;
	fn name(&self) -> &str {
		(*self).name()
	}
}

// Allow &str and String to be used as axis- or device-scope `Setting`s
impl Setting for &str {
	type Type = String;
	fn name(&self) -> &str {
		self
	}
}
impl DeviceScope for &str {}
impl AxisScope for &str {}

impl Setting for String {
	type Type = String;
	fn name(&self) -> &str {
		self.as_str()
	}
}
impl DeviceScope for String {}
impl AxisScope for String {}

/// An interface for getting and/or setting the settings on a single device or axis.
///
/// A [`Settings`] instance is created via the [`Device::settings`](super::Device::settings)
/// and [`Axis::settings`](super::Axis::settings) methods.
#[derive(Debug)]
pub struct Settings<'a, B, P: SharedMut<Port<'a, B>>, S> {
	port: P,
	_info: P::Wrapper<ChainInfo>,
	target: Target,
	_markers: Markers<'a, B>,
	_scope_marker: std::marker::PhantomData<S>,
}

/// A type of [`Settings`] that gives access to axis-scope settings.
///
/// For more details see [`Settings`].
pub type AxisSettings<'a, B, P> = Settings<'a, B, P, AxisScopeMarker>;

impl<'a, B, P> AxisSettings<'a, B, P>
where
	P: SharedMut<Port<'a, B>>,
{
	pub(crate) fn new_axis(axis: &Axis<'a, B, P>) -> Self {
		Settings {
			port: axis.port.clone(),
			_info: axis.info.clone(),
			target: axis.target(),
			_markers: Markers::default(),
			_scope_marker: std::marker::PhantomData,
		}
	}
}

/// A type of [`Settings`] that gives access to device-scope settings.
///
/// For more details see [`Settings`].
pub type DeviceSettings<'a, B, P> = Settings<'a, B, P, DeviceScopeMarker>;

impl<'a, B, P> DeviceSettings<'a, B, P>
where
	P: SharedMut<Port<'a, B>>,
{
	pub(crate) fn new_device(device: &Device<'a, B, P>) -> Self {
		Settings {
			port: device.port.clone(),
			_info: device.info.clone(),
			target: device.target(),
			_markers: Markers::default(),
			_scope_marker: std::marker::PhantomData,
		}
	}
}

impl<'a, B, P, S> Settings<'a, B, P, S>
where
	B: Backend,
	P: SharedMut<Port<'a, B>>,
{
	/// Get the value of a setting.
	///
	/// The reply's warning flag and status fields are not checked. The reply is expected to be "OK".
	pub fn get<T>(&self, setting: T) -> Result<<T::Type as DataType>::Owned, AsciiError>
	where
		T: Setting + SameScopeAsMarker<S>,
	{
		let mut port = self.port.lock_mut().unwrap();
		let reply = port
			.command_reply((self.target, format!("get {}", setting.name())))?
			.flag_ok()?;
		Ok(T::Type::parse_ascii(reply.data())?)
	}

	/// Same as [`Settings::get`] except that the reply is validated with the custom [`Check`](check::Check).
	pub fn get_with_check<T>(
		&self,
		setting: T,
		checker: impl check::Check<Reply>,
	) -> Result<<T::Type as DataType>::Owned, AsciiError>
	where
		T: Setting + SameScopeAsMarker<S>,
	{
		let mut port = self.port.lock_mut().unwrap();
		let reply = port
			.command_reply((self.target, format!("get {}", setting.name())))?
			.check(checker)?;
		Ok(T::Type::parse_ascii(reply.data())?)
	}

	/// Set the value of a setting.
	///
	/// The reply's warning flag and status fields are not checked. The reply is expected to be "OK".
	pub fn set<T, V>(&self, setting: T, value: V) -> Result<(), AsciiError>
	where
		T: Setting + SameScopeAsMarker<S>,
		V: std::borrow::Borrow<<T::Type as DataType>::Borrowed>,
	{
		let mut port = self.port.lock_mut().unwrap();
		let _ = port
			.command_reply((
				self.target,
				format!(
					"set {} {}",
					setting.name(),
					T::Type::display(value.borrow())
				),
			))?
			.flag_ok()?;
		Ok(())
	}

	/// Same as [`Settings::set`] except that the reply is validated with the custom [`Check`](check::Check).
	pub fn set_with_check<T, V>(
		&self,
		setting: T,
		value: V,
		checker: impl check::Check<Reply>,
	) -> Result<(), AsciiError>
	where
		T: Setting + SameScopeAsMarker<S>,
		V: std::borrow::Borrow<<T::Type as DataType>::Borrowed>,
	{
		let mut port = self.port.lock_mut().unwrap();
		let _ = port
			.command_reply((
				self.target,
				format!(
					"set {} {}",
					setting.name(),
					T::Type::display(value.borrow())
				),
			))?
			.check(checker)?;
		Ok(())
	}

	#[cfg(test)]
	pub(crate) fn port(&self) -> &P {
		&self.port
	}
}

#[cfg(test)]
mod test {
	use super::super::test::new_mock_chain;
	use super::*;
	use crate::{
		ascii::{
			scope::{AxisScope, DeviceScope},
			Port,
		},
		backend::Mock,
	};
	use std::{cell::RefCell, rc::Rc};

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

	type SharedPort<'a, B> = Rc<RefCell<Port<'a, B>>>;

	fn new_mock_device_settings() -> DeviceSettings<'static, Mock, SharedPort<'static, Mock>> {
		new_mock_chain(1, 1).device(1).unwrap().settings()
	}
	fn new_mock_axis_settings() -> AxisSettings<'static, Mock, SharedPort<'static, Mock>> {
		new_mock_chain(1, 1)
			.device(1)
			.unwrap()
			.axis(1)
			.unwrap()
			.settings()
	}

	/// Ensure settings of the appropriate scope are accepted by the `get()` and
	/// `set()` methods.
	#[test]
	fn setting_scope_restrictions() {
		let chain = new_mock_chain(1, 1);

		// `get()` accepts appropriately scoped settings and parse the values as the
		// appropriate type
		let device = chain.device(1).unwrap();
		let device_settings = device.settings();
		{
			let mut port = device_settings.port().borrow_mut();
			port.backend_mut().append_data(b"@01 0 OK IDLE FZ 23\r\n");
		}
		let value = device_settings.get(DeviceSetting).unwrap();
		assert_eq!(value, 23u8);

		let axis_settings = device.axis(1).unwrap().settings();
		{
			let mut port = device_settings.port().borrow_mut();
			port.backend_mut().append_data(b"@01 1 OK IDLE FZ 34\r\n");
		}
		let value = axis_settings.get(AxisSetting).unwrap();
		assert_eq!(value, 34u32);

		// `set()` accepts values of the appropriate type.
		let _ = device_settings.set(DeviceSetting, 1u8);
		let _ = axis_settings.set(AxisSetting, 1u32);
	}

	/// Ensure `set()` accepts the appropriate types
	#[test]
	fn set_accepted_values() {
		let settings = new_mock_device_settings();
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
		{
			let settings = new_mock_device_settings();
			let _: Result<String, AsciiError> = settings.get("anything");
			let _: Result<String, AsciiError> = settings.get("anything".to_string());
			let _: Result<String, AsciiError> = settings.get(&"anything".to_string());
		}
		{
			let settings = new_mock_axis_settings();
			let _: Result<String, AsciiError> = settings.get("anything");
			let _: Result<String, AsciiError> = settings.get("anything".to_string());
			let _: Result<String, AsciiError> = settings.get(&"anything".to_string());
		}
	}
}
