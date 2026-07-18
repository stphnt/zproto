//! Types for accessing individual devices/axes on a chain of devices.

pub mod data_type;
pub(crate) mod info;
pub mod iter;
pub mod scope;
pub mod setting;

use crate::{
	ascii::{
		command::Target,
		port::{handlers::Handlers, DefaultTag, Direction},
		response::Alert,
		Port,
	},
	backend::Backend,
	error::AsciiError,
};
use info::ChainInfo;
use iter::{IntoIterAxes, IterAxes, IterDevices};
use setting::{AxisSettings, DeviceSettings};
use std::num::NonZeroU8;

/// Options for creating a [`Chain`] from a port.
///
/// ## Example
///
/// ```
/// # use zproto::ascii::{Port, chain::ChainOptions};
/// # use zproto::backend::Backend;
/// # use zproto::error::AsciiError;
/// # fn wrapper<'a, B: Backend>(mut port: Port<'a, B>) -> Result<(), AsciiError> {
/// let chain = ChainOptions::default()
///     .renumber(true)
///     .build(&mut port)?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Default)]
pub struct ChainOptions {
	/// Whether to renumber the devices in the chain or not.
	renumber: bool,
}

impl ChainOptions {
	/// Whether or not to renumber the devices on the chain while inspecting it.
	///
	/// By default, the chain will not be renumbered.
	pub fn renumber(&mut self, value: bool) -> &mut Self {
		self.renumber = value;
		self
	}

	/// Create a [`Chain`] from the given [`Port`].
	pub fn build<'a, B, Tag, H>(
		&self,
		port: &mut Port<'a, B, Tag, H>,
	) -> Result<Chain<Tag>, AsciiError>
	where
		B: Backend,
		H: Handlers,
		H::PacketHandler: FnMut(&[u8], Direction) + 'a,
		H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'a,
	{
		if self.renumber {
			for result in port.command_replies_until_timeout_iter("renumber")? {
				result?.flag_ok()?;
			}
		}
		Ok(Chain {
			info: ChainInfo::new(port)?,
			tag: std::marker::PhantomData,
		})
	}
}

/// Represents a chain of devices.
#[derive(Debug)]
pub struct Chain<Tag = DefaultTag> {
	info: ChainInfo,
	tag: std::marker::PhantomData<Tag>,
}

impl<Tag> Chain<Tag> {
	/// Get the number of devices in the chain.
	pub fn len(&self) -> usize {
		self.info.devices.len()
	}

	/// Get whether the chain has no devices in it.
	pub fn is_empty(&self) -> bool {
		self.info.devices.is_empty()
	}

	/// Get an iterator over the devices in the chain.
	pub fn iter(&self) -> IterDevices<'_, Tag> {
		IterDevices::new(&self.info)
	}

	/// Get the [`Device`] at the specified address.
	///
	/// Returns `None` if there is no device at that address.
	pub fn device(&self, address: u8) -> Option<Device<'_, Tag>> {
		NonZeroU8::new(address).and_then(|address| {
			let address_exists = self.info.devices.contains_key(&address);
			if address_exists {
				Some(Device::new(&self.info, address))
			} else {
				None
			}
		})
	}
}

impl<'a, Tag> std::iter::IntoIterator for &'a Chain<Tag> {
	type Item = Device<'a, Tag>;
	type IntoIter = IterDevices<'a, Tag>;

	fn into_iter(self) -> Self::IntoIter {
		IterDevices::new(&self.info)
	}
}

impl<Tag> Chain<Tag> {
	/// Create a new [`Chain`] using the default options.
	///
	/// To customize how the chain is built use [`options`](Chain::options).
	/// This is equivalent to:
	///
	/// ```
	/// # use zproto::ascii::{Port, chain::Chain};
	/// # use zproto::backend::Backend;
	/// # use zproto::error::AsciiError;
	/// # fn wrapper<'a, B: Backend>(mut port: Port<'a, B>) -> Result<Chain, AsciiError> {
	/// Chain::options().build(&mut port)
	/// # }
	/// ```
	pub fn new<'a, B, H>(port: &mut Port<'a, B, Tag, H>) -> Result<Self, AsciiError>
	where
		B: Backend,
		H: Handlers,
		H::PacketHandler: FnMut(&[u8], Direction) + 'a,
		H::UnexpectedAlertHandler: FnMut(Alert) -> Result<(), Alert> + 'a,
	{
		ChainOptions::default().build(port)
	}
}

impl Chain<DefaultTag> {
	/// Get a [`ChainOptions`] to customize the creation of a new [`Chain`].
	pub fn options() -> ChainOptions {
		ChainOptions::default()
	}
}

/// Represents a single device (controller or integrated product).
#[derive(Debug)]
pub struct Device<'a, Tag> {
	info: &'a ChainInfo,
	address: NonZeroU8,
	tag: std::marker::PhantomData<Tag>,
}

impl<'a, Tag> Device<'a, Tag> {
	/// Create a new `Device`
	fn new(info: &'a ChainInfo, address: NonZeroU8) -> Self {
		Device {
			info,
			address,
			tag: std::marker::PhantomData,
		}
	}

	/// Get the [`Target`] for the device.
	pub fn target(&self) -> Target {
		self.address.get().into()
	}

	/// Get an iterator over the axes of the device.
	pub fn iter(&self) -> IterAxes<'a, Tag> {
		IterAxes::new(self.info, self.address)
	}

	/// Get access to this device's settings.
	pub fn settings(&self) -> DeviceSettings<'a, Tag> {
		DeviceSettings::new_device(self)
	}

	/// Get the [`Axis`] at the specified axis number (1-based).
	///
	/// Returns None if there is no axis with that number.
	pub fn axis(&self, number: u8) -> Option<Axis<'a, Tag>> {
		NonZeroU8::new(number).and_then(|number| {
			let is_valid_number = self
				.info
				.devices
				.get(&self.address)
				.map(|device_info| device_info.valid_axis_number(number));
			if let Some(true) = is_valid_number {
				Some(Axis::new(self.info, self.address, number))
			} else {
				None
			}
		})
	}
}

impl<Tag> Clone for Device<'_, Tag> {
	fn clone(&self) -> Self {
		Device {
			info: self.info,
			address: self.address,
			tag: std::marker::PhantomData,
		}
	}
}

impl<'a, Tag> std::iter::IntoIterator for Device<'a, Tag> {
	type Item = Axis<'a, Tag>;
	type IntoIter = IntoIterAxes<'a, Tag>;

	fn into_iter(self) -> Self::IntoIter {
		IntoIterAxes::new(self.info, self.address)
	}
}

impl<'a, Tag> std::iter::IntoIterator for &Device<'a, Tag> {
	type Item = Axis<'a, Tag>;
	type IntoIter = IterAxes<'a, Tag>;

	fn into_iter(self) -> Self::IntoIter {
		IterAxes::new(self.info, self.address)
	}
}

/// Represents a single axis on a device.
#[derive(Debug)]
pub struct Axis<'a, Tag> {
	/// Information about the chain
	info: &'a ChainInfo,
	/// The device address
	address: NonZeroU8,
	/// The axis number
	number: NonZeroU8,
	tag: std::marker::PhantomData<Tag>,
}

impl<Tag> Clone for Axis<'_, Tag> {
	fn clone(&self) -> Self {
		Axis {
			info: self.info,
			address: self.address,
			number: self.number,
			tag: std::marker::PhantomData,
		}
	}
}

impl<'a, Tag> Axis<'a, Tag> {
	/// Create an `Axis`
	fn new(info: &'a ChainInfo, address: NonZeroU8, number: NonZeroU8) -> Self {
		Axis {
			info,
			address,
			number,
			tag: std::marker::PhantomData,
		}
	}

	/// Get the [`Target`] for the axis.
	pub fn target(&self) -> Target {
		(self.address.get(), self.number.get()).into()
	}

	/// Get access to this axis's settings.
	pub fn settings(&self) -> AxisSettings<'a, Tag> {
		AxisSettings::new_axis(self)
	}
}

#[cfg(test)]
pub(crate) mod test {
	use super::*;
	use crate::ascii::port::DefaultTag;
	use crate::backend::Mock;
	use crate::error::DuplicateAddressError;

	pub fn new_mock_port_and_chain(
		num_devices: usize,
		num_axes_per_device: usize,
	) -> (Port<'static, Mock>, Chain<DefaultTag>) {
		let mut port = Port::open_mock();
		{
			let backend = port.backend_mut();
			for address in 1..=num_devices {
				backend.push(format!(
					"@{address:0>2} 0 OK IDLE -- {num_axes_per_device}\r\n"
				));
			}
		}
		let chain = port.chain().unwrap();
		(port, chain)
	}

	pub fn new_mock_chain(num_devices: usize, num_axes_per_device: usize) -> Chain<DefaultTag> {
		let (_, chain) = new_mock_port_and_chain(num_devices, num_axes_per_device);
		chain
	}

	#[test]
	fn construction() {
		let _ = Chain::new(&mut Port::open_mock());
	}

	/// Check that duplicate device addresses are reported properly.
	#[test]
	fn duplicate_addresses() {
		let mut port = Port::open_mock();
		port.backend_mut()
			.push(b"@02 0 OK IDLE F0 1\r\n@01 0 OK IDLE WR 3\r\n@02 0 OK IDLE -- 2\r\n");
		assert!(matches!(
			Chain::new(&mut port),
			Err(AsciiError::DuplicateAddress(DuplicateAddressError {
				address: 2
			}))
		));
	}

	/// Check that devices can be iterated over and will be ordered by address
	/// regardless of the order in which the responses are received.
	#[test]
	fn chain_iter() {
		let chain = new_mock_chain(3, 1);
		let addresses: Vec<_> = chain
			.iter()
			.map(|device| device.target().device())
			.collect();
		assert_eq!(addresses, [1, 2, 3]);
	}

	#[test]
	fn chain_into_iter() {
		let chain = new_mock_chain(3, 1);
		{
			let mut addresses = Vec::new();
			for device in &chain {
				addresses.push(device.target().device());
			}
			assert_eq!(addresses, [1, 2, 3]);
		}
	}

	/// Check that axes can be iterated over.
	#[test]
	fn device_iter() {
		let chain = new_mock_chain(1, 3);
		let device = chain.device(1).unwrap();
		let axes: Vec<_> = device.iter().map(|axis| axis.target().axis()).collect();
		assert_eq!(axes, [1, 2, 3]);
	}

	/// Check that axes can be iterated over.
	#[test]
	fn device_into_iter() {
		let chain = new_mock_chain(1, 3);
		let device = chain.device(1).unwrap();
		{
			let mut axes = Vec::new();
			for axis in &device {
				axes.push(axis.target().axis());
			}
			assert_eq!(axes, [1, 2, 3]);
		}
		{
			let mut axes = Vec::new();
			for axis in device {
				axes.push(axis.target().axis());
			}
			assert_eq!(axes, [1, 2, 3]);
		}
	}
}
