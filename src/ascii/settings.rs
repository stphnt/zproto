//! Types representing ASCII settings.

pub mod data_types;

/// Define a type representing an ASCII setting.
#[allow(unused_macros)] // It is used by generated files included below.
macro_rules! define_settings {
    (
        $(
            $(#[$metadata:meta])*
            $name:literal => pub struct $setting:ident: $($value_type:ty),+
        )+
    ) => {
        $(
        $(#[$metadata])*
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $setting;
        impl $setting {
            /// Returns the name of the setting.
            pub const fn name(&self) -> &'static str {
                $name
            }
        }
        impl crate::ascii::settings::Setting for $setting {
            fn name(&self) -> &str {
                self.name()
            }
        }
        $(
        impl crate::ascii::settings::ValueType<$value_type> for $setting {}
        )+
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

        define_any_setting! {
        	/// Any firmware setting.
    		pub enum AnySetting {
        		$(
		    		$(#[$metadata])*
		    		$setting
        		),+
        	}
        }
    };
}

/// Define an enum that represents multiple ASCII settings. Each variant `$setting`
/// must also be the name for another type that implements [`Setting`].
///
/// This macro will automatically defined [`Setting`], the scope, and conversion
/// traits between it and the setting types it maps to.
#[allow(unused_macros)] // It is used by generated files included below.
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
        impl crate::ascii::settings::Setting for $any_setting {
            fn name(&self) -> &str {
                match self {
                    $(
                        Self::$setting => $setting.name()
                    ),+
                }
            }
        }
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

/// Any type that represents a setting
pub trait Setting {
	/// Get the name of the setting.
	fn name(&self) -> &str;
}

impl<T> Setting for &T
where
	T: Setting + ?Sized,
{
	fn name(&self) -> &str {
		(*self).name()
	}
}

/// Marks a type `T` as a datatype for a setting.
pub trait ValueType<T> {}

impl<T, U> ValueType<U> for &T where T: ValueType<U> {}

// Allow strings to be used as settings.
impl Setting for &str {
	fn name(&self) -> &str {
		self
	}
}
impl ValueType<String> for &str {}

impl Setting for String {
	fn name(&self) -> &str {
		self.as_str()
	}
}
impl ValueType<String> for String {}

include!("settings.inc");
