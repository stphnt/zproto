//! Information about a chain, its devices and its axes.

use crate::{
	ascii::{check, Port},
	backend::Backend,
	error::{AsciiCheckDataError, AsciiError, DuplicateAddressError},
};
use std::{collections::BTreeMap, num::NonZeroU8};

/// Information about an entire chain.
#[derive(Debug)]
pub struct ChainInfo {
	/// Information about the devices in the chain.
	pub devices: BTreeMap<NonZeroU8, DeviceInfo>,
}

impl ChainInfo {
	/// Collect all the devices found on the chain
	pub fn new<B: Backend>(port: &mut Port<'_, B>) -> Result<Self, AsciiError> {
		let replies = port
			.command_replies_until_timeout_with_check("get system.axiscount", check::flag_ok())?;
		let mut unique_addresses = std::collections::HashSet::with_capacity(replies.len());
		let mut devices = BTreeMap::new();
		for reply in replies {
			// Replies are always from non-zero addresses so it is safe to unwrap.
			let address: NonZeroU8 = reply.target().device().try_into().unwrap();
			if !unique_addresses.insert(address) {
				return Err(DuplicateAddressError {
					address: address.get(),
				}
				.into());
			}
			let axis_count = reply.data().parse().map_err(|_| {
				AsciiCheckDataError::new(
					"could not parse data as 8-bit unsigned integer",
					reply.into(),
				)
			})?;
			devices.insert(
				address,
				DeviceInfo {
					address,
					axes: Vec::from_iter((0..axis_count).map(|_| AxisInfo {})),
				},
			);
		}
		Ok(ChainInfo { devices })
	}
}

/// Information about a device.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DeviceInfo {
	/// The address of the device
	pub address: NonZeroU8,
	/// Information about the axes.
	pub axes: Vec<AxisInfo>,
}

impl DeviceInfo {
	pub fn valid_axis_number(&self, number: NonZeroU8) -> bool {
		number.get() as usize - 1 < self.axes.len()
	}
}

/// Information about an axis.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AxisInfo {}
