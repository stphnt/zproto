//! Types for working with chains and/or specific devices or axes on a chain.
//!
//! ```
//! # use zproto::ascii::chain
//! let chain = port.into_chain_sync();
//! let sync_device = chain.device(1);
//!
//! ```

use crate::ascii::*;
use crate::error::*;

/// The firmware version of a device.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    /// The major version number
    pub major: u32,
    /// The minor version number
    pub minor: u32,
    /// The build number
    pub build: u32,
}

/// Information about a device.
#[derive(Debug, Clone)]
struct DeviceInfo {
    id: u32,
    serial: u32,
    version: Version,
    axes: Vec<AxisInfo>,
}

/// Information about an axis
#[derive(Debug, Clone)]
struct AxisInfo {
    id: u32,
    serial: Option<u32>,
}

fn parse_setting<T: std::str::FromStr>(
    name: &str,
    reply: Reply,
) -> Result<T, AsciiCheckDataError<AnyResponse>> {
    let value = match reply.data().parse::<T>() {
        Ok(v) => v,
        Err(_) => {
            return Err(crate::error::AsciiCheckDataError::new(
                format!(
                    "could not parse the value of {} ({}) as {}",
                    name,
                    reply.data(),
                    std::any::type_name::<T>()
                ),
                AnyResponse::from(reply),
            ))
        }
    };
    Ok(value)
}

fn parse_setting_multiple<T: std::str::FromStr>(
    name: &str,
    reply: Reply,
) -> Result<Vec<Option<T>>, AsciiCheckDataError<AnyResponse>> {
    let mut values = Vec::new();
    for value in reply.data().split_whitespace() {
        let value = match value.parse::<T>() {
            Ok(v) => Some(v),
            Err(_) => {
                if value == "NA" {
                    None
                } else {
                    return Err(crate::error::AsciiCheckDataError::new(
                        format!(
                            "could not parse the value of {} ({}) as {}",
                            name,
                            value,
                            std::any::type_name::<T>()
                        ),
                        AnyResponse::from(reply),
                    ));
                }
            }
        };
        values.push(value);
    }
    Ok(values)
}

/// Shared access to devices and/or axes on a chain that are thread safe.
pub mod sync {
    use super::*;
    use crate::backend::Backend;
    use std::{
        collections::HashMap,
        sync::{Arc, Mutex},
    };

    type SyncPort<'a, B> = std::sync::Arc<std::sync::Mutex<Port<'a, B>>>;

    /// A collection of all devices in a chain.
    #[derive(Debug)]
    pub struct Chain<'a, B> {
        /// The shared port
        port: SyncPort<'a, B>,
        /// Mapping of device addresses to device info.
        device_infos: HashMap<u8, Arc<DeviceInfo>>,
    }

    impl<'a, B> Chain<'a, B>
    where
        B: Backend,
    {
        pub(crate) fn new(port: Port<'a, B>) -> Result<Self, AsciiError> {
            fn get_and_parse<'b, B: Backend, T: std::str::FromStr>(
                port: &mut Port<'b, B>,
                target: Target,
                setting: &str,
            ) -> Result<T, AsciiError> {
                let reply = port.command_reply_with_check(
                    (target, format!("get {setting}").as_str()),
                    check::flag_ok(),
                )?;
                parse_setting(setting, reply).map_err(From::from)
            }
            fn get_and_parse_multiple<'b, B: Backend, T: std::str::FromStr>(
                port: &mut Port<'b, B>,
                target: Target,
                setting: &str,
            ) -> Result<Vec<Option<T>>, AsciiError> {
                let reply = port.command_reply_with_check(
                    (target, format!("get {setting}")),
                    check::flag_ok(),
                )?;
                parse_setting_multiple(setting, reply).map_err(From::from)
            }

            let port = Arc::new(Mutex::new(port));
            let mut device_infos = HashMap::new();
            {
                // Collect information about the devices in the chain.
                let mut port = port.lock().unwrap();
                let device_id_replies = port.command_replies_until_timeout_with_check(
                    (0, "get deviceid"),
                    check::flag_ok(),
                )?;
                for reply in device_id_replies {
                    let target = reply.target();
                    let id = reply.data().parse().map_err(|_| {
                        crate::error::AsciiCheckDataError::new(
                            format!("could not parse the value of deviceid ({})", reply.data()),
                            AnyResponse::from(reply),
                        )
                    })?;
                    let serial: u32 = get_and_parse(&mut *port, target, "system.serial")?;
                    let version: String = get_and_parse(&mut *port, target, "version")?;
                    let (major, minor) = version
                        .split_once('.')
                        .map(|(major, minor)| {
                            (major.parse::<u32>().unwrap(), minor.parse::<u32>().unwrap())
                        })
                        .unwrap();
                    let build: u32 = get_and_parse(&mut *port, target, "version.build")?;
                    let mut axis_infos = Vec::new();
                    let peripheral_ids = get_and_parse_multiple(&mut *port, target, "peripheralid")
                        .or_else(|_| {
                            get_and_parse_multiple(&mut *port, target, "peripheral.id.integrated")
                        });
                    if let Ok(pids) = peripheral_ids {
                        if let Ok(serials) =
                            get_and_parse_multiple(&mut *port, target, "peripheral.serial.pending")
                        {
                            for (pid, serial) in pids.iter().zip(serials.iter()) {
                                axis_infos.push(AxisInfo {
                                    id: pid.unwrap(),
                                    serial: *serial,
                                })
                            }
                        } else {
                            for pid in pids {
                                axis_infos.push(AxisInfo {
                                    id: pid.unwrap(),
                                    serial: None,
                                })
                            }
                        }
                    }
                    device_infos.insert(
                        target.device(),
                        Arc::new(DeviceInfo {
                            id,
                            serial,
                            version: Version {
                                major,
                                minor,
                                build,
                            },
                            axes: axis_infos,
                        }),
                    );
                }
            }
            Ok(Self { port, device_infos })
        }

        /// Get the device with the specified address
        pub fn device(&self, address: u8) -> Option<Device<'a, B>> {
            self.device_infos.get(&address).map(|info| Device {
                address,
                port: Arc::clone(&self.port),
                info: Arc::clone(info),
            })
        }
    }

    /// A device in a chain.
    #[derive(Debug, Clone)]
    pub struct Device<'a, B> {
        address: u8,
        port: SyncPort<'a, B>,
        info: Arc<DeviceInfo>,
    }

    impl<'a, B> Device<'a, B> {
        /// The address of the device
        pub fn address(&self) -> u8 {
            self.address
        }

        /// Return a reference to an [`Axis`].
        pub fn axis(&self, axis: u8) -> Axis<'a, B> {
            Axis {
                target: Target::for_device(self.address).with_axis(axis),
                port: Arc::clone(&self.port),
                info: Arc::clone(&self.info),
            }
        }
    }

    impl<'a, B> Device<'a, B>
    where
        B: Backend,
    {
        /// Send a command and receive a reply
        pub fn command_reply(&mut self, data: &str) -> Result<Reply, AsciiError> {
            self.port
                .lock()
                .expect("could not mutably access shared port")
                .command_reply((self.address, data))
        }

        /// Get the value of a specified setting.
        ///
        /// The value is automatically converted to the appropriate type for the
        /// setting.
        pub fn get<S>(
            &mut self,
            setting: S,
        ) -> Option<Result<S::Data, AsciiCheckDataError<AnyResponse>>>
        where
            S: setting::Setting + setting::DeviceScope,
        {
            let mut port = self
                .port
                .lock()
                .expect("could not mutably access shared port");
            let reply = port
                .command_reply_with_check(
                    (self.address, format!("get {}", setting.as_ref())),
                    check::flag_ok(),
                )
                .ok()?;
            Some(parse_setting(setting.as_ref(), reply))
        }
    }

    /// A single axis on a device
    #[derive(Debug, Clone)]
    pub struct Axis<'a, B> {
        target: Target,
        port: SyncPort<'a, B>,
        info: Arc<DeviceInfo>,
    }

    impl<'a, B> Axis<'a, B> {
        /// The axis number
        pub fn number(&self) -> u8 {
            self.target.axis()
        }
    }

    impl<'a, B> Axis<'a, B>
    where
        B: Backend,
    {
        /// Send a command and receive a reply
        pub fn command_reply(&self, data: &str) -> Result<Reply, AsciiError> {
            self.port
                .lock()
                .expect("could not mutably access shared port")
                .command_reply((self.target, data))
        }
    }
}
