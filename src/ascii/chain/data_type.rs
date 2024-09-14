//! Types and traits for custom parsing and displaying/formatting of data in ASCII
//! messages.
//!
//! The [`DataType`] trait is the primary trait of interest here, and used by [`Settings`]
//! to parse the appropriate data types when reading settings. All other types
//! traits are there to support [`DataType`].
//!
//! [`Settings`]: crate::ascii::chain::setting::Settings
use crate::ascii::setting::data_types::MacAddress;
use crate::error::ConversionError;

/// Any type that is a valid data type in the ASCII protocol.
pub trait DataType {
	/// The borrowed type.
	type Borrowed: ?Sized;
	/// The owned type.
	type Owned;
	/// The type used to parse an owned value from an ASCII message.
	type Parser: Parse<Output = Self::Owned>;
	/// The type used to format a value in an ASCII message.
	type Displayer<'a>: Display<Input = Self::Borrowed>
	where
		Self: 'a;

	/// Parse the owned version of this data type from a string.
	fn parse(s: &str) -> Result<Self::Owned, ConversionError> {
		Self::Parser::parse(s)
	}
	/// Get an instance of a type for formatting this value in an ASCII message.
	fn display(value: &Self::Borrowed) -> <Self::Displayer<'_> as Display>::Display<'_> {
		Self::Displayer::display(value)
	}
}

macro_rules! impl_data_type {
    (
        $( $type:ty ),+ $(,)?
    ) => {
        $(
            impl DataType for $type {
                type Borrowed = $type;
                type Owned = $type;
                type Parser = $type;
                type Displayer<'a> = $type where Self: 'a;
            }
        )+
    }
}

impl_data_type! { u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f32, f64, std::net::Ipv4Addr, MacAddress }

impl DataType for bool {
	type Borrowed = bool;
	type Owned = bool;
	type Parser = AsciiBool;
	type Displayer<'a> = AsciiBool where Self: 'a;
}

impl DataType for String {
	type Borrowed = str;
	type Owned = String;
	type Parser = String;
	type Displayer<'a> = String where Self: 'a;
}

/// Any type that can parse value of type `Output` from a word in an ASCII message's data field.
pub trait Parse {
	/// The type of the produced value.
	type Output;
	/// Parse a value of type `Output` from a word in an ASCII message.
	fn parse(s: &str) -> Result<Self::Output, ConversionError>;
}

/// Any type that generates another type, `Display`, to properly "display" (write)
/// a value of type `Input` into an ASCII command.
pub trait Display {
	/// The type of the value to be displayed.
	type Input: ?Sized;
	/// The type generated that will display the input type.
	type Display<'a>: std::fmt::Display
	where
		Self::Input: 'a;

	/// Generate a new display type for the value.
	fn display(value: &Self::Input) -> Self::Display<'_>;
}

macro_rules! impl_parse_and_display {
    ( $($type:ty),+ $(,)? ) => {
        $(
            impl Parse for $type {
                type Output = $type;
                fn parse(s: &str) -> Result<Self::Output, ConversionError> {
                    s.parse().map_err(|e| {
                        let msg = format!("could not parse `{}` as {}: {}", s, stringify!($type), e);
                        ConversionError(msg.into_boxed_str())
                    })
                }
            }
            impl Display for $type {
                type Input = $type;
                type Display<'a> = $type where Self::Input: 'a;
                fn display(value: &$type) -> $type {
                    *value
                }
            }
        )+
    }
}
impl_parse_and_display! { u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f32, f64, std::net::Ipv4Addr, MacAddress }

impl Parse for String {
	type Output = String;
	fn parse(s: &str) -> Result<Self::Output, ConversionError> {
		let value = s.trim();
		if value.is_empty() {
			Err(ConversionError(
				"found empty string".to_string().into_boxed_str(),
			))
		} else {
			Ok(value.to_string())
		}
	}
}

impl Display for String {
	type Input = str;
	type Display<'a> = &'a str where Self::Input: 'a;
	fn display(value: &str) -> Self::Display<'_> {
		value
	}
}

/// A type for parsing and formatting a bool in ASCII messages.
#[derive(Debug)]
pub struct AsciiBool(pub bool);

impl Parse for AsciiBool {
	type Output = bool;
	fn parse(s: &str) -> Result<Self::Output, ConversionError> {
		match s {
			"0" => Ok(false),
			"1" => Ok(true),
			other => {
				let msg = format!("could not parse `{other}` as bool");
				Err(ConversionError(msg.into_boxed_str()))
			}
		}
	}
}
impl Display for AsciiBool {
	type Input = bool;
	type Display<'a> = AsciiBool where Self::Input: 'a;
	fn display(value: &bool) -> Self::Display<'_> {
		AsciiBool(*value)
	}
}
impl std::fmt::Display for AsciiBool {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", i32::from(self.0))
	}
}

#[cfg(test)]
mod test {
	use super::*;

	macro_rules! make_parse_display_tests {
        (
            $(
                $type:ident {
                    $( $input:literal => $expected:pat ),+
                    $(,)?
                }
            )+
        ) => {
            $(
                #[allow(non_snake_case)]
                #[test]
                fn $type() {
                    $(
                        let input = $input;
                        let result = <$type as DataType>::parse(input);
                        match result {
                            $expected => {}
                            unexpected => panic!("expected {} but got {:?}", stringify!($expected), unexpected),
                        }
                        if let Ok(value) = result {
                            let formatted = format!("{}", <$type as DataType>::display(&value));
                            assert_eq!(&*formatted, input);
                        }
                    )+
                }
            )+
        };
    }

	make_parse_display_tests! {
		bool {
			"0" => Ok(false),
			"1" => Ok(true),
			"-1" => Err(_),
			"2" => Err(_),
			"true" => Err(_),
			"false" => Err(_),
		}
		u8 {
			"0" => Ok(0),
			"255" => Ok(255),
			"-1" => Err(_),
			"256" => Err(_),
		}
		i8 {
			"-128" => Ok(-128),
			"0" => Ok(0),
			"127" => Ok(127),
			"-129" => Err(_),
			"128" => Err(_),
		}
		u16 {
			"0" => Ok(0),
			"65535" => Ok(65535),
			"-1" => Err(_),
			"65536" => Err(_),
		}
		String {
			"anything" => Ok(_),
			"" => Err(_),
			" " => Err(_),
		}
		MacAddress {
			"FF-FF-FF-FF-FF-FF" => Ok(_),
			"01-23-45-67-89-AB" => Ok(_),
			"00-00-00-00-00-00" => Ok(_),
			"00-00-00-00-00" => Err(_),
			"00-00-00-00-00-00-00" => Err(_),
			"00-00-00-00-00-FG" => Err(_),
		}
	}
}
