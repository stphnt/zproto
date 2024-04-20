//! Iterators over `Chain`s and `Device`s.

use super::{info::ChainInfo, Axis, Device};
use std::num::NonZeroU8;

/// An iterator of the devices in a [`Chain`](super::Chain).
///
/// Created via [`Chain::iter()`](super::Chain::iter).
pub struct IterDevices<'a> {
	info: &'a ChainInfo,
	next_index: usize,
}
impl<'a> IterDevices<'a> {
	/// Create an new iterator over the devices in the chain.
	pub(super) fn new(info: &'a ChainInfo) -> Self {
		IterDevices {
			info,
			next_index: 0,
		}
	}
}

impl<'a> std::fmt::Debug for IterDevices<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("IterDevices").finish_non_exhaustive()
	}
}

impl<'a> std::iter::Iterator for IterDevices<'a> {
	type Item = Device<'a>;
	fn next(&mut self) -> Option<Self::Item> {
		let address = self
			.info
			.devices
			.values()
			.nth(self.next_index)
			.map(|device_info| device_info.address);
		if let Some(address) = address {
			self.next_index += 1;
			Some(Device {
				info: self.info,
				address,
			})
		} else {
			None
		}
	}
}

/// An iterator over the axes of a [`Device`].
///
/// Created via [`Device::iter()`].
pub struct IterAxes<'a> {
	info: &'a ChainInfo,
	address: NonZeroU8,
	next_axis_index: usize,
}

impl<'a> IterAxes<'a> {
	pub(super) fn new(info: &'a ChainInfo, address: NonZeroU8) -> Self {
		IterAxes {
			info,
			address,
			next_axis_index: 0,
		}
	}
}

impl<'a> std::iter::Iterator for IterAxes<'a> {
	type Item = Axis<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		let axis_number = self
			.info
			.devices
			.get(&self.address)
			.and_then(|device_info| {
				if self.next_axis_index < device_info.axes.len() {
					Some(NonZeroU8::new(self.next_axis_index as u8 + 1).unwrap())
				} else {
					None
				}
			});
		if let Some(axis_number) = axis_number {
			self.next_axis_index += 1;
			Some(Axis {
				info: self.info,
				address: self.address,
				axis: axis_number,
			})
		} else {
			None
		}
	}
}

impl<'a> std::fmt::Debug for IterAxes<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("IterAxes")
			.field("address", &self.address)
			.finish()
	}
}

/// A by-value iterator over the axes of a [`Device`].
///
/// Created via [`Device::into_iter()`].
#[derive(Debug)]
pub struct IntoIterAxes<'a> {
	iter: IterAxes<'a>,
}

impl<'a> IntoIterAxes<'a> {
	pub(super) fn new(info: &'a ChainInfo, address: NonZeroU8) -> Self {
		IntoIterAxes {
			iter: IterAxes::new(info, address),
		}
	}
}

impl<'a> std::iter::Iterator for IntoIterAxes<'a> {
	type Item = Axis<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		self.iter.next()
	}
}
