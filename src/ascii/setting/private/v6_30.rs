//! Settings in firmware version 6.30.

// #############################################################################
//  WARNING:
//  This file is generated by scripts/generate-apis. Do not modify it manually.
// #############################################################################

use crate::ascii::setting::Setting;
use crate::ascii::chain::scope::{AxisScope, DeviceScope};

define_settings! {

    /// The type representing the [`accel`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#accel) setting.
    pub struct Accel: Setting<Type = u32, Name = "accel">, AxisScope;
    /// The type representing the [`calibration.type`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#calibration.type) setting.
    pub struct CalibrationType: Setting<Type = u8, Name = "calibration.type">, AxisScope;
    /// The type representing the [`cloop.counts`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.counts) setting.
    pub struct CloopCounts: Setting<Type = u16, Name = "cloop.counts">, AxisScope;
    /// The type representing the [`cloop.displace.tolerance`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.displace.tolerance) setting.
    pub struct CloopDisplaceTolerance: Setting<Type = u32, Name = "cloop.displace.tolerance">, AxisScope;
    /// The type representing the [`cloop.duration.max`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.duration.max) setting.
    pub struct CloopDurationMax: Setting<Type = u16, Name = "cloop.duration.max">, AxisScope;
    /// The type representing the [`cloop.mode`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.mode) setting.
    pub struct CloopMode: Setting<Type = u8, Name = "cloop.mode">, AxisScope;
    /// The type representing the [`cloop.stalltimeout`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.stalltimeout) setting.
    pub struct CloopStalltimeout: Setting<Type = u16, Name = "cloop.stalltimeout">, AxisScope;
    /// The type representing the [`cloop.steps`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.steps) setting.
    pub struct CloopSteps: Setting<Type = u8, Name = "cloop.steps">, AxisScope;
    /// The type representing the [`comm.address`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.address) setting.
    pub struct CommAddress: Setting<Type = u8, Name = "comm.address">, DeviceScope;
    /// The type representing the [`comm.alert`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.alert) setting.
    pub struct CommAlert: Setting<Type = bool, Name = "comm.alert">, DeviceScope;
    /// The type representing the [`comm.checksum`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.checksum) setting.
    pub struct CommChecksum: Setting<Type = bool, Name = "comm.checksum">, DeviceScope;
    /// The type representing the [`comm.protocol`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.protocol) setting.
    pub struct CommProtocol: Setting<Type = u8, Name = "comm.protocol">, DeviceScope;
    /// The type representing the [`comm.rs232.baud`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.rs232.baud) setting.
    pub struct CommRs232Baud: Setting<Type = u32, Name = "comm.rs232.baud">, DeviceScope;
    /// The type representing the [`comm.rs232.protocol`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.rs232.protocol) setting.
    pub struct CommRs232Protocol: Setting<Type = u8, Name = "comm.rs232.protocol">, DeviceScope;
    /// The type representing the [`comm.rs485.baud`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.rs485.baud) setting.
    pub struct CommRs485Baud: Setting<Type = u32, Name = "comm.rs485.baud">, DeviceScope;
    /// The type representing the [`comm.rs485.enable`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.rs485.enable) setting.
    pub struct CommRs485Enable: Setting<Type = bool, Name = "comm.rs485.enable">, DeviceScope;
    /// The type representing the [`comm.rs485.protocol`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.rs485.protocol) setting.
    pub struct CommRs485Protocol: Setting<Type = u8, Name = "comm.rs485.protocol">, DeviceScope;
    /// The type representing the [`comm.usb.protocol`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.usb.protocol) setting.
    pub struct CommUsbProtocol: Setting<Type = u8, Name = "comm.usb.protocol">, DeviceScope;
    /// The type representing the [`deviceid`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#deviceid) setting.
    pub struct Deviceid: Setting<Type = u32, Name = "deviceid">, DeviceScope;
    /// The type representing the [`driver.current.hold`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#driver.current.hold) setting.
    pub struct DriverCurrentHold: Setting<Type = u32, Name = "driver.current.hold">, AxisScope;
    /// The type representing the [`driver.current.max`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#driver.current.max) setting.
    pub struct DriverCurrentMax: Setting<Type = u32, Name = "driver.current.max">, AxisScope;
    /// The type representing the [`driver.current.run`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#driver.current.run) setting.
    pub struct DriverCurrentRun: Setting<Type = u32, Name = "driver.current.run">, AxisScope;
    /// The type representing the [`driver.dir`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#driver.dir) setting.
    pub struct DriverDir: Setting<Type = bool, Name = "driver.dir">, AxisScope;
    /// The type representing the [`driver.temperature`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#driver.temperature) setting.
    pub struct DriverTemperature: Setting<Type = f32, Name = "driver.temperature">, AxisScope;
    /// The type representing the [`encoder.count`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.count) setting.
    pub struct EncoderCount: Setting<Type = i64, Name = "encoder.count">, AxisScope;
    /// The type representing the [`encoder.count.calibrated`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.count.calibrated) setting.
    pub struct EncoderCountCalibrated: Setting<Type = i64, Name = "encoder.count.calibrated">, AxisScope;
    /// The type representing the [`encoder.dir`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.dir) setting.
    pub struct EncoderDir: Setting<Type = bool, Name = "encoder.dir">, AxisScope;
    /// The type representing the [`encoder.error`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.error) setting.
    pub struct EncoderError: Setting<Type = i32, Name = "encoder.error">, AxisScope;
    /// The type representing the [`encoder.fault.type`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.fault.type) setting.
    pub struct EncoderFaultType: Setting<Type = u8, Name = "encoder.fault.type">, AxisScope;
    /// The type representing the [`encoder.filter`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.filter) setting.
    pub struct EncoderFilter: Setting<Type = u16, Name = "encoder.filter">, AxisScope;
    /// The type representing the [`encoder.index.count`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.index.count) setting.
    pub struct EncoderIndexCount: Setting<Type = i16, Name = "encoder.index.count">, AxisScope;
    /// The type representing the [`encoder.index.mode`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.index.mode) setting.
    pub struct EncoderIndexMode: Setting<Type = bool, Name = "encoder.index.mode">, AxisScope;
    /// The type representing the [`encoder.index.phase`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.index.phase) setting.
    pub struct EncoderIndexPhase: Setting<Type = bool, Name = "encoder.index.phase">, AxisScope;
    /// The type representing the [`encoder.mode`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.mode) setting.
    pub struct EncoderMode: Setting<Type = u8, Name = "encoder.mode">, AxisScope;
    /// The type representing the [`encoder.pos`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.pos) setting.
    pub struct EncoderPos: Setting<Type = i32, Name = "encoder.pos">, AxisScope;
    /// The type representing the [`filter.holderid`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#filter.holderid) setting.
    pub struct FilterHolderid: Setting<Type = u16, Name = "filter.holderid">, DeviceScope;
    /// The type representing the [`force.average`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#force.average) setting.
    pub struct ForceAverage: Setting<Type = i16, Name = "force.average">, DeviceScope;
    /// The type representing the [`joy.debug`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#joy.debug) setting.
    pub struct JoyDebug: Setting<Type = bool, Name = "joy.debug">, DeviceScope;
    /// The type representing the [`knob.dir`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.dir) setting.
    pub struct KnobDir: Setting<Type = bool, Name = "knob.dir">, AxisScope;
    /// The type representing the [`knob.distance`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.distance) setting.
    pub struct KnobDistance: Setting<Type = u32, Name = "knob.distance">, AxisScope;
    /// The type representing the [`knob.enable`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.enable) setting.
    pub struct KnobEnable: Setting<Type = bool, Name = "knob.enable">, AxisScope;
    /// The type representing the [`knob.force`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.force) setting.
    pub struct KnobForce: Setting<Type = u32, Name = "knob.force">, AxisScope;
    /// The type representing the [`knob.forceprofile`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.forceprofile) setting.
    pub struct KnobForceprofile: Setting<Type = u8, Name = "knob.forceprofile">, AxisScope;
    /// The type representing the [`knob.maxspeed`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.maxspeed) setting.
    pub struct KnobMaxspeed: Setting<Type = u64, Name = "knob.maxspeed">, AxisScope;
    /// The type representing the [`knob.mode`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.mode) setting.
    pub struct KnobMode: Setting<Type = u8, Name = "knob.mode">, AxisScope;
    /// The type representing the [`knob.speedprofile`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.speedprofile) setting.
    pub struct KnobSpeedprofile: Setting<Type = u8, Name = "knob.speedprofile">, AxisScope;
    /// The type representing the [`limit.approach.maxspeed`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.approach.maxspeed) setting.
    pub struct LimitApproachMaxspeed: Setting<Type = u64, Name = "limit.approach.maxspeed">, AxisScope;
    /// The type representing the [`limit.away.action`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.action) setting.
    pub struct LimitAwayAction: Setting<Type = u8, Name = "limit.away.action">, AxisScope;
    /// The type representing the [`limit.away.edge`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.edge) setting.
    pub struct LimitAwayEdge: Setting<Type = bool, Name = "limit.away.edge">, AxisScope;
    /// The type representing the [`limit.away.pos`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.pos) setting.
    pub struct LimitAwayPos: Setting<Type = i32, Name = "limit.away.pos">, AxisScope;
    /// The type representing the [`limit.away.posupdate`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.posupdate) setting.
    pub struct LimitAwayPosupdate: Setting<Type = u8, Name = "limit.away.posupdate">, AxisScope;
    /// The type representing the [`limit.away.preset`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.preset) setting.
    pub struct LimitAwayPreset: Setting<Type = i32, Name = "limit.away.preset">, AxisScope;
    /// The type representing the [`limit.away.state`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.state) setting.
    pub struct LimitAwayState: Setting<Type = bool, Name = "limit.away.state">, AxisScope;
    /// The type representing the [`limit.away.triggered`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.triggered) setting.
    pub struct LimitAwayTriggered: Setting<Type = bool, Name = "limit.away.triggered">, AxisScope;
    /// The type representing the [`limit.away.type`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.type) setting.
    pub struct LimitAwayType: Setting<Type = u8, Name = "limit.away.type">, AxisScope;
    /// The type representing the [`limit.c.action`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.action) setting.
    pub struct LimitCAction: Setting<Type = u8, Name = "limit.c.action">, AxisScope;
    /// The type representing the [`limit.c.edge`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.edge) setting.
    pub struct LimitCEdge: Setting<Type = bool, Name = "limit.c.edge">, AxisScope;
    /// The type representing the [`limit.c.pos`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.pos) setting.
    pub struct LimitCPos: Setting<Type = i32, Name = "limit.c.pos">, AxisScope;
    /// The type representing the [`limit.c.posupdate`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.posupdate) setting.
    pub struct LimitCPosupdate: Setting<Type = u8, Name = "limit.c.posupdate">, AxisScope;
    /// The type representing the [`limit.c.preset`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.preset) setting.
    pub struct LimitCPreset: Setting<Type = i32, Name = "limit.c.preset">, AxisScope;
    /// The type representing the [`limit.c.state`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.state) setting.
    pub struct LimitCState: Setting<Type = bool, Name = "limit.c.state">, AxisScope;
    /// The type representing the [`limit.c.triggered`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.triggered) setting.
    pub struct LimitCTriggered: Setting<Type = bool, Name = "limit.c.triggered">, AxisScope;
    /// The type representing the [`limit.c.type`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.type) setting.
    pub struct LimitCType: Setting<Type = u8, Name = "limit.c.type">, AxisScope;
    /// The type representing the [`limit.cycle.dist`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.cycle.dist) setting.
    pub struct LimitCycleDist: Setting<Type = u32, Name = "limit.cycle.dist">, AxisScope;
    /// The type representing the [`limit.d.action`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.action) setting.
    pub struct LimitDAction: Setting<Type = u8, Name = "limit.d.action">, AxisScope;
    /// The type representing the [`limit.d.edge`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.edge) setting.
    pub struct LimitDEdge: Setting<Type = bool, Name = "limit.d.edge">, AxisScope;
    /// The type representing the [`limit.d.pos`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.pos) setting.
    pub struct LimitDPos: Setting<Type = i32, Name = "limit.d.pos">, AxisScope;
    /// The type representing the [`limit.d.posupdate`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.posupdate) setting.
    pub struct LimitDPosupdate: Setting<Type = u8, Name = "limit.d.posupdate">, AxisScope;
    /// The type representing the [`limit.d.preset`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.preset) setting.
    pub struct LimitDPreset: Setting<Type = i32, Name = "limit.d.preset">, AxisScope;
    /// The type representing the [`limit.d.state`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.state) setting.
    pub struct LimitDState: Setting<Type = bool, Name = "limit.d.state">, AxisScope;
    /// The type representing the [`limit.d.triggered`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.triggered) setting.
    pub struct LimitDTriggered: Setting<Type = bool, Name = "limit.d.triggered">, AxisScope;
    /// The type representing the [`limit.d.type`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.type) setting.
    pub struct LimitDType: Setting<Type = u8, Name = "limit.d.type">, AxisScope;
    /// The type representing the [`limit.detect.decelonly`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.detect.decelonly) setting.
    pub struct LimitDetectDecelonly: Setting<Type = u32, Name = "limit.detect.decelonly">, AxisScope;
    /// The type representing the [`limit.detect.maxspeed`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.detect.maxspeed) setting.
    pub struct LimitDetectMaxspeed: Setting<Type = u64, Name = "limit.detect.maxspeed">, AxisScope;
    /// The type representing the [`limit.home.action`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.action) setting.
    pub struct LimitHomeAction: Setting<Type = u8, Name = "limit.home.action">, AxisScope;
    /// The type representing the [`limit.home.edge`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.edge) setting.
    pub struct LimitHomeEdge: Setting<Type = bool, Name = "limit.home.edge">, AxisScope;
    /// The type representing the [`limit.home.pos`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.pos) setting.
    pub struct LimitHomePos: Setting<Type = i32, Name = "limit.home.pos">, AxisScope;
    /// The type representing the [`limit.home.posupdate`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.posupdate) setting.
    pub struct LimitHomePosupdate: Setting<Type = u8, Name = "limit.home.posupdate">, AxisScope;
    /// The type representing the [`limit.home.preset`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.preset) setting.
    pub struct LimitHomePreset: Setting<Type = i32, Name = "limit.home.preset">, AxisScope;
    /// The type representing the [`limit.home.state`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.state) setting.
    pub struct LimitHomeState: Setting<Type = bool, Name = "limit.home.state">, AxisScope;
    /// The type representing the [`limit.home.triggered`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.triggered) setting.
    pub struct LimitHomeTriggered: Setting<Type = bool, Name = "limit.home.triggered">, AxisScope;
    /// The type representing the [`limit.home.type`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.type) setting.
    pub struct LimitHomeType: Setting<Type = u8, Name = "limit.home.type">, AxisScope;
    /// The type representing the [`limit.max`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.max) setting.
    pub struct LimitMax: Setting<Type = i32, Name = "limit.max">, AxisScope;
    /// The type representing the [`limit.min`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.min) setting.
    pub struct LimitMin: Setting<Type = i32, Name = "limit.min">, AxisScope;
    /// The type representing the [`limit.start.pos`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.start.pos) setting.
    pub struct LimitStartPos: Setting<Type = u8, Name = "limit.start.pos">, AxisScope;
    /// The type representing the [`limit.swapinputs`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.swapinputs) setting.
    pub struct LimitSwapinputs: Setting<Type = bool, Name = "limit.swapinputs">, AxisScope;
    /// The type representing the [`lockstep.numgroups`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#lockstep.numgroups) setting.
    pub struct LockstepNumgroups: Setting<Type = u8, Name = "lockstep.numgroups">, DeviceScope;
    /// The type representing the [`lockstep.tolerance`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#lockstep.tolerance) setting.
    pub struct LockstepTolerance: Setting<Type = u32, Name = "lockstep.tolerance">, AxisScope;
    /// The type representing the [`maxspeed`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#maxspeed) setting.
    pub struct Maxspeed: Setting<Type = u64, Name = "maxspeed">, AxisScope;
    /// The type representing the [`motion.accelonly`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#motion.accelonly) setting.
    pub struct MotionAccelonly: Setting<Type = u32, Name = "motion.accelonly">, AxisScope;
    /// The type representing the [`motion.decelonly`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#motion.decelonly) setting.
    pub struct MotionDecelonly: Setting<Type = u32, Name = "motion.decelonly">, AxisScope;
    /// The type representing the [`motion.index.dist`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#motion.index.dist) setting.
    pub struct MotionIndexDist: Setting<Type = u32, Name = "motion.index.dist">, AxisScope;
    /// The type representing the [`motion.index.num`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#motion.index.num) setting.
    pub struct MotionIndexNum: Setting<Type = u32, Name = "motion.index.num">, AxisScope;
    /// The type representing the [`peripheral.serial`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#peripheral.serial) setting.
    pub struct PeripheralSerial: Setting<Type = u32, Name = "peripheral.serial">, AxisScope;
    /// The type representing the [`peripheralid`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#peripheralid) setting.
    pub struct Peripheralid: Setting<Type = u32, Name = "peripheralid">, AxisScope;
    /// The type representing the [`pos`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#pos) setting.
    pub struct Pos: Setting<Type = i32, Name = "pos">, AxisScope;
    /// The type representing the [`resolution`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#resolution) setting.
    pub struct Resolution: Setting<Type = u16, Name = "resolution">, AxisScope;
    /// The type representing the [`stream.numbufs`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#stream.numbufs) setting.
    pub struct StreamNumbufs: Setting<Type = u32, Name = "stream.numbufs">, DeviceScope;
    /// The type representing the [`stream.numstreams`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#stream.numstreams) setting.
    pub struct StreamNumstreams: Setting<Type = u32, Name = "stream.numstreams">, DeviceScope;
    /// The type representing the [`system.access`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.access) setting.
    pub struct SystemAccess: Setting<Type = u16, Name = "system.access">, DeviceScope;
    /// The type representing the [`system.axiscount`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.axiscount) setting.
    pub struct SystemAxiscount: Setting<Type = u32, Name = "system.axiscount">, DeviceScope;
    /// The type representing the [`system.current`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.current) setting.
    pub struct SystemCurrent: Setting<Type = f32, Name = "system.current">, DeviceScope;
    /// The type representing the [`system.led.enable`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.led.enable) setting.
    pub struct SystemLedEnable: Setting<Type = bool, Name = "system.led.enable">, DeviceScope;
    /// The type representing the [`system.serial`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.serial) setting.
    pub struct SystemSerial: Setting<Type = u32, Name = "system.serial">, DeviceScope;
    /// The type representing the [`system.temperature`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.temperature) setting.
    pub struct SystemTemperature: Setting<Type = f32, Name = "system.temperature">, DeviceScope;
    /// The type representing the [`system.voltage`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.voltage) setting.
    pub struct SystemVoltage: Setting<Type = f32, Name = "system.voltage">, DeviceScope;
    /// The type representing the [`version`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#version) setting.
    pub struct Version: Setting<Type = f32, Name = "version">, DeviceScope;
    /// The type representing the [`version.build`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#version.build) setting.
    pub struct VersionBuild: Setting<Type = u32, Name = "version.build">, DeviceScope;
    /// The type representing the [`virtual.numvirtual`](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#virtual.numvirtual) setting.
    pub struct VirtualNumvirtual: Setting<Type = u16, Name = "virtual.numvirtual">, DeviceScope;
}
define_any_setting! {
/// Any setting available in firmware version 6.30.
pub enum AnySetting {
    /// The [accel](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#accel) setting.
    Accel,
    /// The [calibration.type](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#calibration.type) setting.
    CalibrationType,
    /// The [cloop.counts](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.counts) setting.
    CloopCounts,
    /// The [cloop.displace.tolerance](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.displace.tolerance) setting.
    CloopDisplaceTolerance,
    /// The [cloop.duration.max](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.duration.max) setting.
    CloopDurationMax,
    /// The [cloop.mode](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.mode) setting.
    CloopMode,
    /// The [cloop.stalltimeout](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.stalltimeout) setting.
    CloopStalltimeout,
    /// The [cloop.steps](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#cloop.steps) setting.
    CloopSteps,
    /// The [comm.address](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.address) setting.
    CommAddress,
    /// The [comm.alert](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.alert) setting.
    CommAlert,
    /// The [comm.checksum](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.checksum) setting.
    CommChecksum,
    /// The [comm.protocol](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.protocol) setting.
    CommProtocol,
    /// The [comm.rs232.baud](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.rs232.baud) setting.
    CommRs232Baud,
    /// The [comm.rs232.protocol](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.rs232.protocol) setting.
    CommRs232Protocol,
    /// The [comm.rs485.baud](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.rs485.baud) setting.
    CommRs485Baud,
    /// The [comm.rs485.enable](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.rs485.enable) setting.
    CommRs485Enable,
    /// The [comm.rs485.protocol](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.rs485.protocol) setting.
    CommRs485Protocol,
    /// The [comm.usb.protocol](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#comm.usb.protocol) setting.
    CommUsbProtocol,
    /// The [deviceid](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#deviceid) setting.
    Deviceid,
    /// The [driver.current.hold](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#driver.current.hold) setting.
    DriverCurrentHold,
    /// The [driver.current.max](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#driver.current.max) setting.
    DriverCurrentMax,
    /// The [driver.current.run](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#driver.current.run) setting.
    DriverCurrentRun,
    /// The [driver.dir](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#driver.dir) setting.
    DriverDir,
    /// The [driver.temperature](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#driver.temperature) setting.
    DriverTemperature,
    /// The [encoder.count](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.count) setting.
    EncoderCount,
    /// The [encoder.count.calibrated](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.count.calibrated) setting.
    EncoderCountCalibrated,
    /// The [encoder.dir](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.dir) setting.
    EncoderDir,
    /// The [encoder.error](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.error) setting.
    EncoderError,
    /// The [encoder.fault.type](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.fault.type) setting.
    EncoderFaultType,
    /// The [encoder.filter](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.filter) setting.
    EncoderFilter,
    /// The [encoder.index.count](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.index.count) setting.
    EncoderIndexCount,
    /// The [encoder.index.mode](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.index.mode) setting.
    EncoderIndexMode,
    /// The [encoder.index.phase](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.index.phase) setting.
    EncoderIndexPhase,
    /// The [encoder.mode](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.mode) setting.
    EncoderMode,
    /// The [encoder.pos](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#encoder.pos) setting.
    EncoderPos,
    /// The [filter.holderid](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#filter.holderid) setting.
    FilterHolderid,
    /// The [force.average](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#force.average) setting.
    ForceAverage,
    /// The [joy.debug](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#joy.debug) setting.
    JoyDebug,
    /// The [knob.dir](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.dir) setting.
    KnobDir,
    /// The [knob.distance](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.distance) setting.
    KnobDistance,
    /// The [knob.enable](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.enable) setting.
    KnobEnable,
    /// The [knob.force](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.force) setting.
    KnobForce,
    /// The [knob.forceprofile](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.forceprofile) setting.
    KnobForceprofile,
    /// The [knob.maxspeed](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.maxspeed) setting.
    KnobMaxspeed,
    /// The [knob.mode](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.mode) setting.
    KnobMode,
    /// The [knob.speedprofile](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#knob.speedprofile) setting.
    KnobSpeedprofile,
    /// The [limit.approach.maxspeed](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.approach.maxspeed) setting.
    LimitApproachMaxspeed,
    /// The [limit.away.action](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.action) setting.
    LimitAwayAction,
    /// The [limit.away.edge](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.edge) setting.
    LimitAwayEdge,
    /// The [limit.away.pos](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.pos) setting.
    LimitAwayPos,
    /// The [limit.away.posupdate](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.posupdate) setting.
    LimitAwayPosupdate,
    /// The [limit.away.preset](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.preset) setting.
    LimitAwayPreset,
    /// The [limit.away.state](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.state) setting.
    LimitAwayState,
    /// The [limit.away.triggered](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.triggered) setting.
    LimitAwayTriggered,
    /// The [limit.away.type](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.away.type) setting.
    LimitAwayType,
    /// The [limit.c.action](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.action) setting.
    LimitCAction,
    /// The [limit.c.edge](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.edge) setting.
    LimitCEdge,
    /// The [limit.c.pos](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.pos) setting.
    LimitCPos,
    /// The [limit.c.posupdate](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.posupdate) setting.
    LimitCPosupdate,
    /// The [limit.c.preset](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.preset) setting.
    LimitCPreset,
    /// The [limit.c.state](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.state) setting.
    LimitCState,
    /// The [limit.c.triggered](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.triggered) setting.
    LimitCTriggered,
    /// The [limit.c.type](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.c.type) setting.
    LimitCType,
    /// The [limit.cycle.dist](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.cycle.dist) setting.
    LimitCycleDist,
    /// The [limit.d.action](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.action) setting.
    LimitDAction,
    /// The [limit.d.edge](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.edge) setting.
    LimitDEdge,
    /// The [limit.d.pos](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.pos) setting.
    LimitDPos,
    /// The [limit.d.posupdate](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.posupdate) setting.
    LimitDPosupdate,
    /// The [limit.d.preset](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.preset) setting.
    LimitDPreset,
    /// The [limit.d.state](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.state) setting.
    LimitDState,
    /// The [limit.d.triggered](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.triggered) setting.
    LimitDTriggered,
    /// The [limit.d.type](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.d.type) setting.
    LimitDType,
    /// The [limit.detect.decelonly](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.detect.decelonly) setting.
    LimitDetectDecelonly,
    /// The [limit.detect.maxspeed](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.detect.maxspeed) setting.
    LimitDetectMaxspeed,
    /// The [limit.home.action](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.action) setting.
    LimitHomeAction,
    /// The [limit.home.edge](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.edge) setting.
    LimitHomeEdge,
    /// The [limit.home.pos](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.pos) setting.
    LimitHomePos,
    /// The [limit.home.posupdate](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.posupdate) setting.
    LimitHomePosupdate,
    /// The [limit.home.preset](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.preset) setting.
    LimitHomePreset,
    /// The [limit.home.state](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.state) setting.
    LimitHomeState,
    /// The [limit.home.triggered](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.triggered) setting.
    LimitHomeTriggered,
    /// The [limit.home.type](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.home.type) setting.
    LimitHomeType,
    /// The [limit.max](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.max) setting.
    LimitMax,
    /// The [limit.min](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.min) setting.
    LimitMin,
    /// The [limit.start.pos](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.start.pos) setting.
    LimitStartPos,
    /// The [limit.swapinputs](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#limit.swapinputs) setting.
    LimitSwapinputs,
    /// The [lockstep.numgroups](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#lockstep.numgroups) setting.
    LockstepNumgroups,
    /// The [lockstep.tolerance](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#lockstep.tolerance) setting.
    LockstepTolerance,
    /// The [maxspeed](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#maxspeed) setting.
    Maxspeed,
    /// The [motion.accelonly](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#motion.accelonly) setting.
    MotionAccelonly,
    /// The [motion.decelonly](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#motion.decelonly) setting.
    MotionDecelonly,
    /// The [motion.index.dist](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#motion.index.dist) setting.
    MotionIndexDist,
    /// The [motion.index.num](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#motion.index.num) setting.
    MotionIndexNum,
    /// The [peripheral.serial](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#peripheral.serial) setting.
    PeripheralSerial,
    /// The [peripheralid](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#peripheralid) setting.
    Peripheralid,
    /// The [pos](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#pos) setting.
    Pos,
    /// The [resolution](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#resolution) setting.
    Resolution,
    /// The [stream.numbufs](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#stream.numbufs) setting.
    StreamNumbufs,
    /// The [stream.numstreams](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#stream.numstreams) setting.
    StreamNumstreams,
    /// The [system.access](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.access) setting.
    SystemAccess,
    /// The [system.axiscount](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.axiscount) setting.
    SystemAxiscount,
    /// The [system.current](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.current) setting.
    SystemCurrent,
    /// The [system.led.enable](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.led.enable) setting.
    SystemLedEnable,
    /// The [system.serial](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.serial) setting.
    SystemSerial,
    /// The [system.temperature](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.temperature) setting.
    SystemTemperature,
    /// The [system.voltage](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#system.voltage) setting.
    SystemVoltage,
    /// The [version](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#version) setting.
    Version,
    /// The [version.build](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#version.build) setting.
    VersionBuild,
    /// The [virtual.numvirtual](https://www.zaber.com/w/Manuals/ASCII_Protocol_Manual#virtual.numvirtual) setting.
    VirtualNumvirtual,
}
}