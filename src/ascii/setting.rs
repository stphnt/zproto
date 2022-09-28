//! ASCII settings

/// A marker trait indicating that a setting is device-scope.
pub trait DeviceScope {}

/// A marker trait indicating that a setting is axis-scope.
pub trait AxisScope {}

/// Represents a type that can be used as an ASCII protocol setting.
pub trait Setting: std::convert::AsRef<str> {
    /// The type of a value of this setting.
    type Data: std::str::FromStr;
}

macro_rules! define_settings {
    (
        $(
            $scope:ident $identifier:ident : $type: ty = $name:literal;
        )+
    ) => {
        $(
            #[doc = concat!("The '", $name, "' setting.")]
            #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
            pub struct $identifier{}
            impl ::std::convert::AsRef<str> for $identifier {
                fn as_ref(&self) -> &str {
                    $name
                }
            }
            impl Setting for $identifier {
                type Data = $type;
            }
            define_settings!{@impl_scope $scope $identifier}
        )+
    };
    (@impl_scope axis $identifier:ident) => {
        impl AxisScope for $identifier {}
    };
    (@impl_scope device $identifier:ident) => {
        impl DeviceScope for $identifier {}
    };
}

define_settings! {
    device DeviceId : u32 = "device.id";
    device DeviceIdDeprecated  : u32 = "deviceid";
    device SystemSerial : u32 = "system.serial";
    axis PeripheralId : u32 = "peripheral.id";
    axis PeripheralIdDeprecated : u32 = "peripheralid";
}
