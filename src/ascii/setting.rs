//! Types representing ASCII settings.

use crate::ascii::{
	chain::scope::{AxisScope, DeviceScope},
	data_type::DataType,
};
pub mod data_types;

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
        impl crate::ascii::setting::Setting for $setting {
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
        impl crate::ascii::setting::Setting for $any_setting {
            type Type = String;
            fn name(&self) -> &str {
                match self {
                    $(
                        Self::$setting => $setting.name()
                    ),+
                }
            }
        }
        impl crate::ascii::chain::scope::AxisScope for $any_setting {}
        impl crate::ascii::chain::scope::DeviceScope for $any_setting {}
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
