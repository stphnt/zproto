//! Types and traits for accessing device or axis settings

use crate::{
    ascii::{
        check,
        data_type::DataType,
        marker::Markers,
        scope::{AxisScope, AxisScopeMarker, DeviceScope, DeviceScopeMarker, SameScopeAsMarker},
        Port, Reply, Target,
    },
    backend::Backend,
    error::AsciiError,
    shared::SharedMut,
};
use std::num::NonZeroU8;

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
#[derive(Debug)]
pub struct Settings<'a, B, P, S> {
    port: P,
    target: Target,
    _markers: Markers<'a, B>,
    _scope_marker: std::marker::PhantomData<S>,
}

/// A type of [`Settings`] that gives access to axis-scope settings.
///
/// For more details see [`Settings`].
pub type AxisSettings<'a, B, P> = Settings<'a, B, P, AxisScopeMarker>;

impl<'a, B, P> AxisSettings<'a, B, P> {
    pub(crate) fn new_axis(address: NonZeroU8, axis: NonZeroU8, port: P) -> Self {
        Settings {
            port,
            target: Target::for_device(address.get()).with_axis(axis.get()),
            _markers: Markers::default(),
            _scope_marker: std::marker::PhantomData,
        }
    }
}

/// A type of [`Settings`] that gives access to device-scope settings.
///
/// For more details see [`Settings`].
pub type DeviceSettings<'a, B, P> = Settings<'a, B, P, DeviceScopeMarker>;

impl<'a, B, P> DeviceSettings<'a, B, P> {
    pub(crate) fn new_device(address: NonZeroU8, port: P) -> Self {
        Settings {
            port,
            target: Target::for_device(address.get()),
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
        let reply = port.command_reply_with_check(
            (self.target, format!("get {}", setting.name())),
            check::flag_ok(),
        )?;
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
            .command_reply_with_check((self.target, format!("get {}", setting.name())), checker)?;
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
        let _ = port.command_reply_with_check(
            (
                self.target,
                format!(
                    "set {} {}",
                    setting.name(),
                    T::Type::display(value.borrow())
                ),
            ),
            check::flag_ok(),
        )?;
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
        let _ = port.command_reply_with_check(
            (
                self.target,
                format!(
                    "set {} {}",
                    setting.name(),
                    T::Type::display(value.borrow())
                ),
            ),
            checker,
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
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

    const ONE: NonZeroU8 = unsafe { NonZeroU8::new_unchecked(1) };

    fn make_mock_device_settings() -> DeviceSettings<'static, Mock, SharedPort<'static, Mock>> {
        let port = Rc::new(RefCell::new(Port::open_mock()));
        DeviceSettings::new_device(ONE, port)
    }
    fn make_mock_axis_settings() -> AxisSettings<'static, Mock, SharedPort<'static, Mock>> {
        let port = Rc::new(RefCell::new(Port::open_mock()));
        AxisSettings::new_axis(ONE, ONE, port)
    }

    /// Ensure settings of the appropriate scope are accepted by the `get()` and
    /// `set()` methods.
    #[test]
    fn setting_scope_restrictions() {
        let mut port = Port::open_mock();
        {
            let backend = port.backend_mut();
            backend.append_data(b"@01 0 OK IDLE FZ 23\r\n");
            backend.append_data(b"@01 1 OK IDLE FZ 34\r\n");
        }
        let port = Rc::new(RefCell::new(port));
        // `get()` accepts appropriately scoped settings and parse the values as the
        // appropriate type
        let device_settings = DeviceSettings::new_device(ONE, port.clone());
        let value = device_settings.get(DeviceSetting).unwrap();
        assert_eq!(value, 23u8);

        let axis_settings = AxisSettings::new_axis(ONE, ONE, port.clone());
        let value = axis_settings.get(AxisSetting).unwrap();
        assert_eq!(value, 34u32);

        // `set()` accepts values of the appropriate type.
        let _ = device_settings.set(DeviceSetting, 1u8);
        let _ = axis_settings.set(AxisSetting, 1u32);
    }

    /// Ensure `set()` accepts the appropriate types
    #[test]
    fn set_accepted_values() {
        let settings = make_mock_device_settings();
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
            let settings = make_mock_device_settings();
            let _: Result<String, AsciiError> = settings.get("anything");
            let _: Result<String, AsciiError> = settings.get("anything".to_string());
            let _: Result<String, AsciiError> = settings.get(&"anything".to_string());
        }
        {
            let settings = make_mock_axis_settings();
            let _: Result<String, AsciiError> = settings.get("anything");
            let _: Result<String, AsciiError> = settings.get("anything".to_string());
            let _: Result<String, AsciiError> = settings.get(&"anything".to_string());
        }
    }
}
