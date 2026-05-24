//! Custom data types for ASCII settings or command parameters.

use crate::ascii::serialization;
use crate::error::ConversionError;

/// A MAC address.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct MacAddress {
	/// The octets in transmission order.
	octets: [u8; 6],
}

impl std::fmt::Display for MacAddress {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		for (i, octet) in self.octets.iter().enumerate() {
			if i != 0 {
				write!(f, "-")?;
			}
			write!(f, "{octet:02X}")?;
		}
		Ok(())
	}
}

impl std::str::FromStr for MacAddress {
	type Err = ConversionError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let mut octets = [0u8; 6];
		let mut max_octet_index = 0;
		for (i, part) in s.split('-').enumerate() {
			max_octet_index = i;
			if i < 6 {
				octets[i] = u8::from_str_radix(part, 16)
					.map_err(|err| ConversionError::new_from::<Self>(s, &err))?;
			} else {
				return Err(ConversionError::new::<Self>(s));
			}
		}
		if max_octet_index != 5 {
			return Err(ConversionError::new::<Self>(s));
		}
		Ok(MacAddress { octets })
	}
}

impl serialization::Serialize for MacAddress {
	fn serialize(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{self}")
	}
}

impl serialization::Deserialize for MacAddress {
	type Error = <Self as std::str::FromStr>::Err;
	fn deserialize(s: &str) -> Result<Self, Self::Error> {
		s.parse()
	}
}

/// A firmware version number.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Version {
	/// The major firmware version number.
	pub major: u8,
	/// The minor firmware version number.
	pub minor: u8,
}

impl std::fmt::Display for Version {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}.{:02}", self.major, self.minor)
	}
}

impl std::str::FromStr for Version {
	type Err = ConversionError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let (major, minor) = s
			.split_once('.')
			.ok_or_else(|| ConversionError::new::<Self>(s))?;
		Ok(Version {
			major: major
				.parse()
				.map_err(|err| ConversionError::new_from::<Self>(s, &err))?,
			minor: minor
				.parse()
				.map_err(|err| ConversionError::new_from::<Self>(s, &err))?,
		})
	}
}

impl serialization::Serialize for Version {
	fn serialize(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{self}")
	}
}

impl serialization::Deserialize for Version {
	type Error = <Self as std::str::FromStr>::Err;
	fn deserialize(s: &str) -> Result<Self, Self::Error> {
		s.parse()
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn mac_address_parse_display() {
		let cases = &[
			// input              expected display (if valid)
			("00-11-22-33-44-55", Ok("00-11-22-33-44-55")),
			("aa-bb-cc-dd-ee-ff", Ok("AA-BB-CC-DD-EE-FF")),
			("", Err(())),
			("001122334455", Err(())),
			("00-11-22-33-44-zz", Err(())),
			("00-11-22-33-44-55-66", Err(())),
			("00-11-22-33-44", Err(())),
		];
		for (i, (input, expected)) in cases.iter().enumerate() {
			eprintln!("case {i}: {input}");
			let result = input.parse::<MacAddress>();
			match expected {
				Ok(display) => assert_eq!(&result.unwrap().to_string(), display),
				Err(()) => assert!(result.is_err()),
			}
		}
	}

	#[test]
	fn version_parse() {
		assert_eq!(
			"7.01".parse::<Version>().unwrap(),
			Version { major: 7, minor: 1 }
		);
		assert_eq!(
			"7.15".parse::<Version>().unwrap(),
			Version {
				major: 7,
				minor: 15,
			}
		);
	}

	#[test]
	fn version_display() {
		assert_eq!(Version { major: 7, minor: 1 }.to_string(), "7.01");
		assert_eq!(
			Version {
				major: 7,
				minor: 15
			}
			.to_string(),
			"7.15"
		);
	}
}
