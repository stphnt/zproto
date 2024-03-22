//! Custom data types for ASCII settings.

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
	type Err = InvalidMacAddress;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let mut octets = [0u8; 6];
		let mut max_octet_index = 0;
		for (i, part) in s.split('-').enumerate() {
			max_octet_index = i;
			if i < 6 {
				octets[i] = u8::from_str_radix(part, 16).map_err(|_| InvalidMacAddress)?;
			} else {
				return Err(InvalidMacAddress);
			}
		}
		if max_octet_index != 5 {
			return Err(InvalidMacAddress);
		}
		Ok(MacAddress { octets })
	}
}

/// Error indicating parsing a string into a [`MacAddress`] failed.
#[derive(Debug, Copy, Clone)]
pub struct InvalidMacAddress;
impl std::fmt::Display for InvalidMacAddress {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "invalid MAC address")
	}
}
impl std::error::Error for InvalidMacAddress {}
