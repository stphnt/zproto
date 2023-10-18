//! Iterators over `Chain`s and `Device`s.

use super::{info::ChainInfo, Axis, Device};
use crate::{
    ascii::{marker::Markers, Port},
    shared::{Shared as _, SharedMut},
};
use std::num::NonZeroU8;

/// An iterator of the devices in a [`Chain`](super::Chain).
pub struct ChainIter<'a, B, P: SharedMut<Port<'a, B>>> {
    port: P,
    info: P::Wrapper<ChainInfo>,
    next_index: usize,
}
impl<'a, B, P> ChainIter<'a, B, P>
where
    P: SharedMut<Port<'a, B>>,
{
    /// Create an new iterator over the devices in the chain.
    pub(super) fn new(port: P, info: P::Wrapper<ChainInfo>) -> Self {
        ChainIter {
            port,
            info,
            next_index: 0,
        }
    }
}

impl<'a, B, P> std::fmt::Debug for ChainIter<'a, B, P>
where
    P: SharedMut<Port<'a, B>>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ChainIter").finish_non_exhaustive()
    }
}

impl<'a, B, P> std::iter::Iterator for ChainIter<'a, B, P>
where
    P: SharedMut<Port<'a, B>>,
{
    type Item = Device<'a, B, P>;
    fn next(&mut self) -> Option<Self::Item> {
        let address = self
            .info
            .lock()
            .unwrap()
            .devices
            .values()
            .nth(self.next_index)
            .map(|device_info| device_info.address);
        if let Some(address) = address {
            self.next_index += 1;
            Some(Device {
                port: self.port.clone(),
                info: self.info.clone(),
                address,
            })
        } else {
            None
        }
    }
}

/// A by-value iterator over the devices in a [`Chain`](super::Chain).
pub struct ChainIntoIter<'a, B, P: SharedMut<Port<'a, B>>> {
    iter: ChainIter<'a, B, P>,
}
impl<'a, B, P> ChainIntoIter<'a, B, P>
where
    P: SharedMut<Port<'a, B>>,
{
    pub(super) fn new(port: P, info: P::Wrapper<ChainInfo>) -> Self {
        ChainIntoIter {
            iter: ChainIter::new(port, info),
        }
    }
}

impl<'a, B, P> std::fmt::Debug for ChainIntoIter<'a, B, P>
where
    P: SharedMut<Port<'a, B>>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ChainIntoIter").finish_non_exhaustive()
    }
}

impl<'a, B, P> std::iter::Iterator for ChainIntoIter<'a, B, P>
where
    P: SharedMut<Port<'a, B>>,
{
    type Item = Device<'a, B, P>;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

/// An iterator over the axes of a [`Device`].
pub struct DeviceIter<'a, B, P: SharedMut<Port<'a, B>>> {
    port: P,
    info: P::Wrapper<ChainInfo>,
    address: NonZeroU8,
    next_axis_index: usize,
}

impl<'a, B, P> DeviceIter<'a, B, P>
where
    P: SharedMut<Port<'a, B>>,
{
    pub(super) fn new(port: P, info: P::Wrapper<ChainInfo>, address: NonZeroU8) -> Self {
        DeviceIter {
            port,
            info,
            address,
            next_axis_index: 0,
        }
    }
}

impl<'a, B, P> std::iter::Iterator for DeviceIter<'a, B, P>
where
    P: SharedMut<Port<'a, B>>,
{
    type Item = Axis<'a, B, P>;

    fn next(&mut self) -> Option<Self::Item> {
        let axis_number = self
            .info
            .lock()
            .unwrap()
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
                port: self.port.clone(),
                info: self.info.clone(),
                address: self.address,
                axis: axis_number,
                _markers: Markers::default(),
            })
        } else {
            None
        }
    }
}

impl<'a, B, P> std::fmt::Debug for DeviceIter<'a, B, P>
where
    P: SharedMut<Port<'a, B>>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DeviceIter")
            .field("address", &self.address)
            .finish()
    }
}

/// A by-value iterator over the axes of a [`Device`].
#[derive(Debug)]
pub struct DeviceIntoIter<'a, B, P: SharedMut<Port<'a, B>>> {
    iter: DeviceIter<'a, B, P>,
}

impl<'a, B, P> DeviceIntoIter<'a, B, P>
where
    P: SharedMut<Port<'a, B>>,
{
    pub(super) fn new(port: P, info: P::Wrapper<ChainInfo>, address: NonZeroU8) -> Self {
        DeviceIntoIter {
            iter: DeviceIter::new(port, info, address),
        }
    }
}

impl<'a, B, P> std::iter::Iterator for DeviceIntoIter<'a, B, P>
where
    P: SharedMut<Port<'a, B>>,
{
    type Item = Axis<'a, B, P>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}
