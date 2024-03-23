//! Types for accessing individual devices/axes on a chain of devices.

pub mod data_type;
pub(crate) mod info;
pub mod iter;
pub mod scope;
pub mod setting;

use crate::{
	ascii::{command::Target, marker::Markers, Port},
	backend::{Backend, Serial},
	error::AsciiError,
	shared::{Shared, SharedMut},
};
use info::ChainInfo;
use iter::{IntoIterAxes, IntoIterDevices, IterAxes, IterDevices};
use setting::{AxisSettings, DeviceSettings};
use std::{
	cell::RefCell,
	num::NonZeroU8,
	rc::Rc,
	sync::{Arc, Mutex},
};

/// Options for creating a [`Chain`] from a port.
///
/// ## Example
///
/// ```
/// # use zproto::ascii::{Port, chain::ChainOptions};
/// # use zproto::backend::Backend;
/// # use zproto::error::AsciiError;
/// # fn wrapper<'a, B: Backend>(port: Port<'a, B>) -> Result<(), AsciiError> {
/// let chain = ChainOptions::default()
///     .renumber(true)
///     .build(port)?;
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
	///
	/// To create a `Chain` that can be shared across threads, use [`build_sync`](ChainOptions::build_sync).
	pub fn build<'a, B: Backend>(&self, mut port: Port<'a, B>) -> Result<Chain<'a, B>, AsciiError> {
		if self.renumber {
			for result in port.command_replies_until_timeout_iter("renumber")? {
				result?.flag_ok()?;
			}
		}
		Ok(Chain {
			info: Rc::new(RefCell::new(ChainInfo::new(&mut port)?)),
			port: Rc::new(RefCell::new(port)),
		})
	}

	/// Create a [`SyncChain`], a `Chain` that can be shared across threads,
	/// from the given [`Port`].
	///
	/// To create a `Chain` that cannot be shared across threads, but has a
	/// reduced overhead, use [`build`](ChainOptions::build).
	pub fn build_sync<'a, B: Backend>(
		&self,
		mut port: Port<'a, B>,
	) -> Result<SyncChain<'a, B>, AsciiError> {
		if self.renumber {
			for result in port.command_replies_until_timeout_iter("renumber")? {
				result?.flag_ok()?;
			}
		}
		Ok(Chain {
			info: Arc::new(Mutex::new(ChainInfo::new(&mut port)?)),
			port: Arc::new(Mutex::new(port)),
		})
	}
}

/// Represents a chain of devices.
#[derive(Debug)]
pub struct Chain<'a, B, P: SharedMut<Port<'a, B>> = Rc<RefCell<Port<'a, B>>>> {
	port: P,
	info: P::Wrapper<ChainInfo>,
}

impl<'a, B, P> Chain<'a, B, P>
where
	P: SharedMut<Port<'a, B>>,
{
	/// Get the number of devices in the chain.
	pub fn len(&self) -> usize {
		self.info.lock().unwrap().devices.len()
	}

	/// Get whether the chain has no devices in it.
	pub fn is_empty(&self) -> bool {
		self.info.lock().unwrap().devices.is_empty()
	}

	/// Get an iterator over the devices in the chain.
	pub fn iter(&self) -> IterDevices<'a, B, P> {
		IterDevices::new(self.port.clone(), self.info.clone())
	}

	/// Get the [`Device`] at the specified address.
	///
	/// Returns `None` if there is no device at that address.
	pub fn device(&self, address: u8) -> Option<Device<'a, B, P>> {
		NonZeroU8::new(address).and_then(|address| {
			let address_exists = self.info.lock().unwrap().devices.contains_key(&address);
			if address_exists {
				Some(Device {
					port: self.port.clone(),
					info: self.info.clone(),
					address,
				})
			} else {
				None
			}
		})
	}
}

impl<'a, B, P> std::iter::IntoIterator for Chain<'a, B, P>
where
	P: SharedMut<Port<'a, B>>,
{
	type Item = Device<'a, B, P>;
	type IntoIter = IntoIterDevices<'a, B, P>;

	fn into_iter(self) -> Self::IntoIter {
		IntoIterDevices::new(self.port, self.info)
	}
}

impl<'a, B, P> std::iter::IntoIterator for &Chain<'a, B, P>
where
	P: SharedMut<Port<'a, B>>,
{
	type Item = Device<'a, B, P>;
	type IntoIter = IterDevices<'a, B, P>;

	fn into_iter(self) -> Self::IntoIter {
		IterDevices::new(self.port.clone(), self.info.clone())
	}
}

impl Chain<'static, Port<'static, Serial>> {
	/// Get a [`ChainOptions`] to customize the creation of a new [`Chain`].
	pub fn options() -> ChainOptions {
		ChainOptions::default()
	}
}

impl<'a, B: Backend> Chain<'a, B> {
	/// Create a new [`Chain`] using the default options.
	///
	/// To customize how the chain is built use [`options`](Chain::options).
	/// This is equivalent to:
	///
	/// ```
	/// # use zproto::ascii::{Port, chain::Chain};
	/// # use zproto::backend::Backend;
	/// # use zproto::error::AsciiError;
	/// # fn wrapper<'a, B: Backend>(port: Port<'a, B>) -> Result<Chain<'a, B>, AsciiError> {
	/// Chain::options().build(port)
	/// # }
	/// ```
	///
	/// To create a `Chain` that can be shared across threads, use [`new_sync`](Chain::new_sync).
	pub fn new(port: Port<'a, B>) -> Result<Self, AsciiError> {
		ChainOptions::default().build(port)
	}
}

/// A [`Chain`] that can shared between threads.
pub type SyncChain<'a, B> = Chain<'a, B, Arc<Mutex<Port<'a, B>>>>;

impl<'a, B: Backend> SyncChain<'a, B> {
	/// Create a new [`Chain`] that can be shared across threads using the default options.
	///
	/// To customize how the chain is built use [`options`](Chain::options).
	/// This is equivalent to:
	///
	/// ```
	/// # use zproto::ascii::{Port, chain::{Chain, SyncChain}};
	/// # use zproto::backend::Backend;
	/// # use zproto::error::AsciiError;
	/// # fn wrapper<'a, B: Backend>(port: Port<'a, B>) -> Result<SyncChain<'a, B>, AsciiError> {
	/// Chain::options().build_sync(port)
	/// # }
	/// ```
	///
	/// To create a `Chain` that cannot be shared across threads, but has
	/// reduced overhead, use [`new`](Chain::new).
	pub fn new_sync(port: Port<'a, B>) -> Result<Self, AsciiError> {
		ChainOptions::default().build_sync(port)
	}
}

/// Represents a single device (controller or integrated product).
#[derive(Debug)]
pub struct Device<'a, B, P: SharedMut<Port<'a, B>> = Rc<RefCell<Port<'a, B>>>> {
	port: P,
	info: P::Wrapper<ChainInfo>,
	address: NonZeroU8,
}

/// A [`Device`] that can shared between threads.
pub type SyncDevice<'a, B> = Device<'a, B, Arc<Mutex<Port<'a, B>>>>;

impl<'a, B, P> Device<'a, B, P>
where
	P: SharedMut<Port<'a, B>>,
{
	/// Get the [`Target`] for the device.
	pub fn target(&self) -> Target {
		self.address.get().into()
	}

	/// Get an iterator over the axes of the device.
	pub fn iter(&self) -> IterAxes<'a, B, P> {
		IterAxes::new(self.port.clone(), self.info.clone(), self.address)
	}

	/// Get access to this device's settings.
	pub fn settings(&self) -> DeviceSettings<'a, B, P> {
		DeviceSettings::new_device(self)
	}

	/// Get the [`Axis`] at the specified axis number (1-based).
	///
	/// Returns None if there is no axis with that number.
	pub fn axis(&self, number: u8) -> Option<Axis<'a, B, P>> {
		NonZeroU8::new(number).and_then(|number| {
			let is_valid_number = self
				.info
				.lock()
				.unwrap()
				.devices
				.get(&self.address)
				.map(|device_info| device_info.valid_axis_number(number));
			if let Some(true) = is_valid_number {
				Some(Axis {
					port: self.port.clone(),
					info: self.info.clone(),
					address: self.address,
					axis: number,
					_markers: Markers::default(),
				})
			} else {
				None
			}
		})
	}
}

impl<'a, B, P> Clone for Device<'a, B, P>
where
	P: SharedMut<Port<'a, B>>,
{
	fn clone(&self) -> Self {
		Device {
			port: self.port.clone(),
			info: self.info.clone(),
			address: self.address,
		}
	}
}

impl<'a, B, P> std::iter::IntoIterator for Device<'a, B, P>
where
	P: SharedMut<Port<'a, B>>,
{
	type Item = Axis<'a, B, P>;
	type IntoIter = IntoIterAxes<'a, B, P>;

	fn into_iter(self) -> Self::IntoIter {
		IntoIterAxes::new(self.port, self.info, self.address)
	}
}

impl<'a, B, P> std::iter::IntoIterator for &Device<'a, B, P>
where
	P: SharedMut<Port<'a, B>>,
{
	type Item = Axis<'a, B, P>;
	type IntoIter = IterAxes<'a, B, P>;

	fn into_iter(self) -> Self::IntoIter {
		IterAxes::new(self.port.clone(), self.info.clone(), self.address)
	}
}

/// Represents a single axis on a device.
#[derive(Debug)]
pub struct Axis<'a, B, P: SharedMut<Port<'a, B>>> {
	/// The shared port
	port: P,
	/// Information about the chain
	info: P::Wrapper<ChainInfo>,
	/// The device address
	address: NonZeroU8,
	/// The axis number
	axis: NonZeroU8,
	_markers: Markers<'a, B>,
}

/// An [`Axis`] that can shared between threads.
pub type SyncAxis<'a, B> = Axis<'a, B, Arc<Mutex<Port<'a, B>>>>;

impl<'a, B, P> Clone for Axis<'a, B, P>
where
	P: SharedMut<Port<'a, B>>,
{
	fn clone(&self) -> Self {
		Axis {
			port: self.port.clone(),
			info: self.info.clone(),
			address: self.address,
			axis: self.axis,
			_markers: Markers::default(),
		}
	}
}

impl<'a, B, P> Axis<'a, B, P>
where
	P: SharedMut<Port<'a, B>>,
{
	/// Get the [`Target`] for the axis.
	pub fn target(&self) -> Target {
		(self.address.get(), self.axis.get()).into()
	}

	/// Get access to this axis's settings.
	pub fn settings(&self) -> AxisSettings<'a, B, P> {
		AxisSettings::new_axis(self)
	}
}

#[cfg(test)]
pub(crate) mod test {
	use super::*;
	use crate::backend::Mock;
	use crate::error::DuplicateAddressError;

	pub fn new_mock_chain(
		num_devices: usize,
		num_axes_per_device: usize,
	) -> Chain<'static, Mock, Rc<RefCell<Port<'static, Mock>>>> {
		let mut port = Port::open_mock();
		{
			let backend = port.backend_mut();
			for address in 1..=num_devices {
				backend.append_data(
					format!("@0{} 0 OK IDLE -- {}\r\n", address, num_axes_per_device).as_bytes(),
				);
			}
		}
		port.into_chain().unwrap()
	}

	#[test]
	fn construction() {
		let _ = Chain::new(Port::open_mock());
		let _ = Chain::new_sync(Port::open_mock());
	}

	/// Check that duplicate device addresses are reported properly.
	#[test]
	fn duplicate_addresses() {
		let mut port = Port::open_mock();
		port.backend_mut()
			.append_data(b"@02 0 OK IDLE F0 1\r\n@01 0 OK IDLE WR 3\r\n@02 0 OK IDLE -- 2\r\n");
		assert!(matches!(
			Chain::new(port),
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
		{
			let mut addresses = Vec::new();
			for device in chain {
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
