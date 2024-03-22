//! Settings in firmware version 7.18.

// #############################################################################
//  WARNING:
//  This file is generated by scripts/generate-apis. Do not modify it manually.
// #############################################################################

use crate::ascii::setting::Setting;
use crate::ascii::chain::scope::{AxisScope, DeviceScope};

define_settings! {

    /// The type of the [`accel`](https://www.zaber.com/protocol-manual#topic_setting_accel) setting.
    pub struct Accel: Setting<Type = u32, Name = "accel">, AxisScope;
    /// The type of the [`calibration.type`](https://www.zaber.com/protocol-manual#topic_setting_calibration_type) setting.
    pub struct CalibrationType: Setting<Type = u8, Name = "calibration.type">, AxisScope;
    /// The type of the [`cloop.continuous.enable`](https://www.zaber.com/protocol-manual#topic_setting_cloop_continuous_enable) setting.
    pub struct CloopContinuousEnable: Setting<Type = bool, Name = "cloop.continuous.enable">, AxisScope;
    /// The type of the [`cloop.displace.tolerance`](https://www.zaber.com/protocol-manual#topic_setting_cloop_displace_tolerance) setting.
    pub struct CloopDisplaceTolerance: Setting<Type = u32, Name = "cloop.displace.tolerance">, AxisScope;
    /// The type of the [`cloop.enable`](https://www.zaber.com/protocol-manual#topic_setting_cloop_enable) setting.
    pub struct CloopEnable: Setting<Type = bool, Name = "cloop.enable">, AxisScope;
    /// The type of the [`cloop.recovery.enable`](https://www.zaber.com/protocol-manual#topic_setting_cloop_recovery_enable) setting.
    pub struct CloopRecoveryEnable: Setting<Type = bool, Name = "cloop.recovery.enable">, AxisScope;
    /// The type of the [`cloop.settle.period`](https://www.zaber.com/protocol-manual#topic_setting_cloop_settle_period) setting.
    pub struct CloopSettlePeriod: Setting<Type = u32, Name = "cloop.settle.period">, AxisScope;
    /// The type of the [`cloop.settle.tolerance`](https://www.zaber.com/protocol-manual#topic_setting_cloop_settle_tolerance) setting.
    pub struct CloopSettleTolerance: Setting<Type = u32, Name = "cloop.settle.tolerance">, AxisScope;
    /// The type of the [`cloop.timeout`](https://www.zaber.com/protocol-manual#topic_setting_cloop_timeout) setting.
    pub struct CloopTimeout: Setting<Type = u16, Name = "cloop.timeout">, AxisScope;
    /// The type of the [`comm.address`](https://www.zaber.com/protocol-manual#topic_setting_comm_address) setting.
    pub struct CommAddress: Setting<Type = u8, Name = "comm.address">, DeviceScope;
    /// The type of the [`comm.alert`](https://www.zaber.com/protocol-manual#topic_setting_comm_alert) setting.
    pub struct CommAlert: Setting<Type = bool, Name = "comm.alert">, DeviceScope;
    /// The type of the [`comm.checksum`](https://www.zaber.com/protocol-manual#topic_setting_comm_checksum) setting.
    pub struct CommChecksum: Setting<Type = bool, Name = "comm.checksum">, DeviceScope;
    /// The type of the [`comm.protocol`](https://www.zaber.com/protocol-manual#topic_setting_comm_protocol) setting.
    pub struct CommProtocol: Setting<Type = u8, Name = "comm.protocol">, DeviceScope;
    /// The type of the [`comm.rs232.baud`](https://www.zaber.com/protocol-manual#topic_setting_comm_rs232_baud) setting.
    pub struct CommRs232Baud: Setting<Type = u32, Name = "comm.rs232.baud">, DeviceScope;
    /// The type of the [`device.id`](https://www.zaber.com/protocol-manual#topic_setting_device_id) setting.
    pub struct DeviceId: Setting<Type = u32, Name = "device.id">, DeviceScope;
    /// The type of the [`driver.current.hold`](https://www.zaber.com/protocol-manual#topic_setting_driver_current_hold) setting.
    pub struct DriverCurrentHold: Setting<Type = u32, Name = "driver.current.hold">, AxisScope;
    /// The type of the [`driver.current.max`](https://www.zaber.com/protocol-manual#topic_setting_driver_current_max) setting.
    pub struct DriverCurrentMax: Setting<Type = u32, Name = "driver.current.max">, AxisScope;
    /// The type of the [`driver.current.run`](https://www.zaber.com/protocol-manual#topic_setting_driver_current_run) setting.
    pub struct DriverCurrentRun: Setting<Type = u32, Name = "driver.current.run">, AxisScope;
    /// The type of the [`driver.current.servo`](https://www.zaber.com/protocol-manual#topic_setting_driver_current_servo) setting.
    pub struct DriverCurrentServo: Setting<Type = u32, Name = "driver.current.servo">, AxisScope;
    /// The type of the [`driver.dir`](https://www.zaber.com/protocol-manual#topic_setting_driver_dir) setting.
    pub struct DriverDir: Setting<Type = bool, Name = "driver.dir">, AxisScope;
    /// The type of the [`driver.enabled`](https://www.zaber.com/protocol-manual#topic_setting_driver_enabled) setting.
    pub struct DriverEnabled: Setting<Type = bool, Name = "driver.enabled">, AxisScope;
    /// The type of the [`driver.temperature`](https://www.zaber.com/protocol-manual#topic_setting_driver_temperature) setting.
    pub struct DriverTemperature: Setting<Type = f32, Name = "driver.temperature">, AxisScope;
    /// The type of the [`encoder.count`](https://www.zaber.com/protocol-manual#topic_setting_encoder_count) setting.
    pub struct EncoderCount: Setting<Type = i64, Name = "encoder.count">, AxisScope;
    /// The type of the [`encoder.count.cal`](https://www.zaber.com/protocol-manual#topic_setting_encoder_count_cal) setting.
    pub struct EncoderCountCal: Setting<Type = i64, Name = "encoder.count.cal">, AxisScope;
    /// The type of the [`encoder.dir`](https://www.zaber.com/protocol-manual#topic_setting_encoder_dir) setting.
    pub struct EncoderDir: Setting<Type = bool, Name = "encoder.dir">, AxisScope;
    /// The type of the [`encoder.pos`](https://www.zaber.com/protocol-manual#topic_setting_encoder_pos) setting.
    pub struct EncoderPos: Setting<Type = i32, Name = "encoder.pos">, AxisScope;
    /// The type of the [`encoder.pos.error`](https://www.zaber.com/protocol-manual#topic_setting_encoder_pos_error) setting.
    pub struct EncoderPosError: Setting<Type = i32, Name = "encoder.pos.error">, AxisScope;
    /// The type of the [`filter.holderid`](https://www.zaber.com/protocol-manual#topic_setting_filter_holderid) setting.
    pub struct FilterHolderid: Setting<Type = u16, Name = "filter.holderid">, DeviceScope;
    /// The type of the [`knob.dir`](https://www.zaber.com/protocol-manual#topic_setting_knob_dir) setting.
    pub struct KnobDir: Setting<Type = bool, Name = "knob.dir">, AxisScope;
    /// The type of the [`knob.distance`](https://www.zaber.com/protocol-manual#topic_setting_knob_distance) setting.
    pub struct KnobDistance: Setting<Type = u32, Name = "knob.distance">, AxisScope;
    /// The type of the [`knob.enable`](https://www.zaber.com/protocol-manual#topic_setting_knob_enable) setting.
    pub struct KnobEnable: Setting<Type = bool, Name = "knob.enable">, AxisScope;
    /// The type of the [`knob.maxspeed`](https://www.zaber.com/protocol-manual#topic_setting_knob_maxspeed) setting.
    pub struct KnobMaxspeed: Setting<Type = u64, Name = "knob.maxspeed">, AxisScope;
    /// The type of the [`knob.mode`](https://www.zaber.com/protocol-manual#topic_setting_knob_mode) setting.
    pub struct KnobMode: Setting<Type = u8, Name = "knob.mode">, AxisScope;
    /// The type of the [`knob.speedprofile`](https://www.zaber.com/protocol-manual#topic_setting_knob_speedprofile) setting.
    pub struct KnobSpeedprofile: Setting<Type = u8, Name = "knob.speedprofile">, AxisScope;
    /// The type of the [`lamp.current`](https://www.zaber.com/protocol-manual#topic_setting_lamp_current) setting.
    pub struct LampCurrent: Setting<Type = f32, Name = "lamp.current">, AxisScope;
    /// The type of the [`lamp.current.max`](https://www.zaber.com/protocol-manual#topic_setting_lamp_current_max) setting.
    pub struct LampCurrentMax: Setting<Type = f32, Name = "lamp.current.max">, AxisScope;
    /// The type of the [`lamp.flux`](https://www.zaber.com/protocol-manual#topic_setting_lamp_flux) setting.
    pub struct LampFlux: Setting<Type = f32, Name = "lamp.flux">, AxisScope;
    /// The type of the [`lamp.flux.max`](https://www.zaber.com/protocol-manual#topic_setting_lamp_flux_max) setting.
    pub struct LampFluxMax: Setting<Type = f32, Name = "lamp.flux.max">, AxisScope;
    /// The type of the [`lamp.status`](https://www.zaber.com/protocol-manual#topic_setting_lamp_status) setting.
    pub struct LampStatus: Setting<Type = u32, Name = "lamp.status">, AxisScope;
    /// The type of the [`lamp.temperature`](https://www.zaber.com/protocol-manual#topic_setting_lamp_temperature) setting.
    pub struct LampTemperature: Setting<Type = f32, Name = "lamp.temperature">, AxisScope;
    /// The type of the [`lamp.wavelength.fwhm`](https://www.zaber.com/protocol-manual#topic_setting_lamp_wavelength_fwhm) setting.
    pub struct LampWavelengthFwhm: Setting<Type = u32, Name = "lamp.wavelength.fwhm">, AxisScope;
    /// The type of the [`lamp.wavelength.peak`](https://www.zaber.com/protocol-manual#topic_setting_lamp_wavelength_peak) setting.
    pub struct LampWavelengthPeak: Setting<Type = u32, Name = "lamp.wavelength.peak">, AxisScope;
    /// The type of the [`limit.approach.maxspeed`](https://www.zaber.com/protocol-manual#topic_setting_limit_approach_maxspeed) setting.
    pub struct LimitApproachMaxspeed: Setting<Type = u64, Name = "limit.approach.maxspeed">, AxisScope;
    /// The type of the [`limit.away.action`](https://www.zaber.com/protocol-manual#topic_setting_limit_away_action) setting.
    pub struct LimitAwayAction: Setting<Type = u8, Name = "limit.away.action">, AxisScope;
    /// The type of the [`limit.away.posupdate`](https://www.zaber.com/protocol-manual#topic_setting_limit_away_posupdate) setting.
    pub struct LimitAwayPosupdate: Setting<Type = u8, Name = "limit.away.posupdate">, AxisScope;
    /// The type of the [`limit.away.preset`](https://www.zaber.com/protocol-manual#topic_setting_limit_away_preset) setting.
    pub struct LimitAwayPreset: Setting<Type = i32, Name = "limit.away.preset">, AxisScope;
    /// The type of the [`limit.away.state`](https://www.zaber.com/protocol-manual#topic_setting_limit_away_state) setting.
    pub struct LimitAwayState: Setting<Type = bool, Name = "limit.away.state">, AxisScope;
    /// The type of the [`limit.away.triggered`](https://www.zaber.com/protocol-manual#topic_setting_limit_away_triggered) setting.
    pub struct LimitAwayTriggered: Setting<Type = bool, Name = "limit.away.triggered">, AxisScope;
    /// The type of the [`limit.c.action`](https://www.zaber.com/protocol-manual#topic_setting_limit_c_action) setting.
    pub struct LimitCAction: Setting<Type = u8, Name = "limit.c.action">, AxisScope;
    /// The type of the [`limit.c.pos`](https://www.zaber.com/protocol-manual#topic_setting_limit_c_pos) setting.
    pub struct LimitCPos: Setting<Type = i32, Name = "limit.c.pos">, AxisScope;
    /// The type of the [`limit.c.posupdate`](https://www.zaber.com/protocol-manual#topic_setting_limit_c_posupdate) setting.
    pub struct LimitCPosupdate: Setting<Type = u8, Name = "limit.c.posupdate">, AxisScope;
    /// The type of the [`limit.c.preset`](https://www.zaber.com/protocol-manual#topic_setting_limit_c_preset) setting.
    pub struct LimitCPreset: Setting<Type = i32, Name = "limit.c.preset">, AxisScope;
    /// The type of the [`limit.c.state`](https://www.zaber.com/protocol-manual#topic_setting_limit_c_state) setting.
    pub struct LimitCState: Setting<Type = bool, Name = "limit.c.state">, AxisScope;
    /// The type of the [`limit.c.triggered`](https://www.zaber.com/protocol-manual#topic_setting_limit_c_triggered) setting.
    pub struct LimitCTriggered: Setting<Type = bool, Name = "limit.c.triggered">, AxisScope;
    /// The type of the [`limit.cycle.dist`](https://www.zaber.com/protocol-manual#topic_setting_limit_cycle_dist) setting.
    pub struct LimitCycleDist: Setting<Type = u32, Name = "limit.cycle.dist">, AxisScope;
    /// The type of the [`limit.detect.decelonly`](https://www.zaber.com/protocol-manual#topic_setting_limit_detect_decelonly) setting.
    pub struct LimitDetectDecelonly: Setting<Type = u32, Name = "limit.detect.decelonly">, AxisScope;
    /// The type of the [`limit.detect.maxspeed`](https://www.zaber.com/protocol-manual#topic_setting_limit_detect_maxspeed) setting.
    pub struct LimitDetectMaxspeed: Setting<Type = u64, Name = "limit.detect.maxspeed">, AxisScope;
    /// The type of the [`limit.home.action`](https://www.zaber.com/protocol-manual#topic_setting_limit_home_action) setting.
    pub struct LimitHomeAction: Setting<Type = u8, Name = "limit.home.action">, AxisScope;
    /// The type of the [`limit.home.posupdate`](https://www.zaber.com/protocol-manual#topic_setting_limit_home_posupdate) setting.
    pub struct LimitHomePosupdate: Setting<Type = u8, Name = "limit.home.posupdate">, AxisScope;
    /// The type of the [`limit.home.preset`](https://www.zaber.com/protocol-manual#topic_setting_limit_home_preset) setting.
    pub struct LimitHomePreset: Setting<Type = i32, Name = "limit.home.preset">, AxisScope;
    /// The type of the [`limit.home.state`](https://www.zaber.com/protocol-manual#topic_setting_limit_home_state) setting.
    pub struct LimitHomeState: Setting<Type = bool, Name = "limit.home.state">, AxisScope;
    /// The type of the [`limit.home.triggered`](https://www.zaber.com/protocol-manual#topic_setting_limit_home_triggered) setting.
    pub struct LimitHomeTriggered: Setting<Type = bool, Name = "limit.home.triggered">, AxisScope;
    /// The type of the [`limit.max`](https://www.zaber.com/protocol-manual#topic_setting_limit_max) setting.
    pub struct LimitMax: Setting<Type = i32, Name = "limit.max">, AxisScope;
    /// The type of the [`limit.min`](https://www.zaber.com/protocol-manual#topic_setting_limit_min) setting.
    pub struct LimitMin: Setting<Type = i32, Name = "limit.min">, AxisScope;
    /// The type of the [`limit.start.pos`](https://www.zaber.com/protocol-manual#topic_setting_limit_start_pos) setting.
    pub struct LimitStartPos: Setting<Type = u8, Name = "limit.start.pos">, AxisScope;
    /// The type of the [`lockstep.numgroups`](https://www.zaber.com/protocol-manual#topic_setting_lockstep_numgroups) setting.
    pub struct LockstepNumgroups: Setting<Type = u8, Name = "lockstep.numgroups">, DeviceScope;
    /// The type of the [`maxspeed`](https://www.zaber.com/protocol-manual#topic_setting_maxspeed) setting.
    pub struct Maxspeed: Setting<Type = u64, Name = "maxspeed">, AxisScope;
    /// The type of the [`motion.accelonly`](https://www.zaber.com/protocol-manual#topic_setting_motion_accelonly) setting.
    pub struct MotionAccelonly: Setting<Type = u32, Name = "motion.accelonly">, AxisScope;
    /// The type of the [`motion.busy`](https://www.zaber.com/protocol-manual#topic_setting_motion_busy) setting.
    pub struct MotionBusy: Setting<Type = bool, Name = "motion.busy">, AxisScope;
    /// The type of the [`motion.decelonly`](https://www.zaber.com/protocol-manual#topic_setting_motion_decelonly) setting.
    pub struct MotionDecelonly: Setting<Type = u32, Name = "motion.decelonly">, AxisScope;
    /// The type of the [`motion.index.dist`](https://www.zaber.com/protocol-manual#topic_setting_motion_index_dist) setting.
    pub struct MotionIndexDist: Setting<Type = u32, Name = "motion.index.dist">, AxisScope;
    /// The type of the [`motion.index.num`](https://www.zaber.com/protocol-manual#topic_setting_motion_index_num) setting.
    pub struct MotionIndexNum: Setting<Type = u32, Name = "motion.index.num">, AxisScope;
    /// The type of the [`motor.current.max`](https://www.zaber.com/protocol-manual#topic_setting_motor_current_max) setting.
    pub struct MotorCurrentMax: Setting<Type = u32, Name = "motor.current.max">, AxisScope;
    /// The type of the [`parking.state`](https://www.zaber.com/protocol-manual#topic_setting_parking_state) setting.
    pub struct ParkingState: Setting<Type = bool, Name = "parking.state">, AxisScope;
    /// The type of the [`peripheral.id`](https://www.zaber.com/protocol-manual#topic_setting_peripheral_id) setting.
    pub struct PeripheralId: Setting<Type = u32, Name = "peripheral.id">, AxisScope;
    /// The type of the [`peripheral.id.pending`](https://www.zaber.com/protocol-manual#topic_setting_peripheral_id_pending) setting.
    pub struct PeripheralIdPending: Setting<Type = u32, Name = "peripheral.id.pending">, AxisScope;
    /// The type of the [`peripheral.serial`](https://www.zaber.com/protocol-manual#topic_setting_peripheral_serial) setting.
    pub struct PeripheralSerial: Setting<Type = u32, Name = "peripheral.serial">, AxisScope;
    /// The type of the [`peripheral.serial.pending`](https://www.zaber.com/protocol-manual#topic_setting_peripheral_serial_pending) setting.
    pub struct PeripheralSerialPending: Setting<Type = u32, Name = "peripheral.serial.pending">, AxisScope;
    /// The type of the [`pos`](https://www.zaber.com/protocol-manual#topic_setting_pos) setting.
    pub struct Pos: Setting<Type = i32, Name = "pos">, AxisScope;
    /// The type of the [`resolution`](https://www.zaber.com/protocol-manual#topic_setting_resolution) setting.
    pub struct Resolution: Setting<Type = u16, Name = "resolution">, AxisScope;
    /// The type of the [`scope.delay`](https://www.zaber.com/protocol-manual#topic_setting_scope_delay) setting.
    pub struct ScopeDelay: Setting<Type = f32, Name = "scope.delay">, DeviceScope;
    /// The type of the [`scope.timebase`](https://www.zaber.com/protocol-manual#topic_setting_scope_timebase) setting.
    pub struct ScopeTimebase: Setting<Type = f32, Name = "scope.timebase">, DeviceScope;
    /// The type of the [`stream.numbufs`](https://www.zaber.com/protocol-manual#topic_setting_stream_numbufs) setting.
    pub struct StreamNumbufs: Setting<Type = u32, Name = "stream.numbufs">, DeviceScope;
    /// The type of the [`stream.numstreams`](https://www.zaber.com/protocol-manual#topic_setting_stream_numstreams) setting.
    pub struct StreamNumstreams: Setting<Type = u32, Name = "stream.numstreams">, DeviceScope;
    /// The type of the [`system.access`](https://www.zaber.com/protocol-manual#topic_setting_system_access) setting.
    pub struct SystemAccess: Setting<Type = u16, Name = "system.access">, DeviceScope;
    /// The type of the [`system.axiscount`](https://www.zaber.com/protocol-manual#topic_setting_system_axiscount) setting.
    pub struct SystemAxiscount: Setting<Type = u32, Name = "system.axiscount">, DeviceScope;
    /// The type of the [`system.led.enable`](https://www.zaber.com/protocol-manual#topic_setting_system_led_enable) setting.
    pub struct SystemLedEnable: Setting<Type = bool, Name = "system.led.enable">, DeviceScope;
    /// The type of the [`system.serial`](https://www.zaber.com/protocol-manual#topic_setting_system_serial) setting.
    pub struct SystemSerial: Setting<Type = u32, Name = "system.serial">, DeviceScope;
    /// The type of the [`system.temperature`](https://www.zaber.com/protocol-manual#topic_setting_system_temperature) setting.
    pub struct SystemTemperature: Setting<Type = f32, Name = "system.temperature">, DeviceScope;
    /// The type of the [`system.voltage`](https://www.zaber.com/protocol-manual#topic_setting_system_voltage) setting.
    pub struct SystemVoltage: Setting<Type = f32, Name = "system.voltage">, DeviceScope;
    /// The type of the [`trigger.numactions`](https://www.zaber.com/protocol-manual#topic_setting_trigger_numactions) setting.
    pub struct TriggerNumactions: Setting<Type = u32, Name = "trigger.numactions">, DeviceScope;
    /// The type of the [`trigger.numtriggers`](https://www.zaber.com/protocol-manual#topic_setting_trigger_numtriggers) setting.
    pub struct TriggerNumtriggers: Setting<Type = u32, Name = "trigger.numtriggers">, DeviceScope;
    /// The type of the [`version`](https://www.zaber.com/protocol-manual#topic_setting_version) setting.
    pub struct Version: Setting<Type = f32, Name = "version">, DeviceScope;
    /// The type of the [`version.build`](https://www.zaber.com/protocol-manual#topic_setting_version_build) setting.
    pub struct VersionBuild: Setting<Type = u32, Name = "version.build">, DeviceScope;
}
define_any_setting! {
/// Any setting available in firmware version 7.18.
pub enum AnySetting {
    /// The [accel](https://www.zaber.com/protocol-manual#topic_setting_accel) setting.
    Accel,
    /// The [calibration.type](https://www.zaber.com/protocol-manual#topic_setting_calibration_type) setting.
    CalibrationType,
    /// The [cloop.continuous.enable](https://www.zaber.com/protocol-manual#topic_setting_cloop_continuous_enable) setting.
    CloopContinuousEnable,
    /// The [cloop.displace.tolerance](https://www.zaber.com/protocol-manual#topic_setting_cloop_displace_tolerance) setting.
    CloopDisplaceTolerance,
    /// The [cloop.enable](https://www.zaber.com/protocol-manual#topic_setting_cloop_enable) setting.
    CloopEnable,
    /// The [cloop.recovery.enable](https://www.zaber.com/protocol-manual#topic_setting_cloop_recovery_enable) setting.
    CloopRecoveryEnable,
    /// The [cloop.settle.period](https://www.zaber.com/protocol-manual#topic_setting_cloop_settle_period) setting.
    CloopSettlePeriod,
    /// The [cloop.settle.tolerance](https://www.zaber.com/protocol-manual#topic_setting_cloop_settle_tolerance) setting.
    CloopSettleTolerance,
    /// The [cloop.timeout](https://www.zaber.com/protocol-manual#topic_setting_cloop_timeout) setting.
    CloopTimeout,
    /// The [comm.address](https://www.zaber.com/protocol-manual#topic_setting_comm_address) setting.
    CommAddress,
    /// The [comm.alert](https://www.zaber.com/protocol-manual#topic_setting_comm_alert) setting.
    CommAlert,
    /// The [comm.checksum](https://www.zaber.com/protocol-manual#topic_setting_comm_checksum) setting.
    CommChecksum,
    /// The [comm.protocol](https://www.zaber.com/protocol-manual#topic_setting_comm_protocol) setting.
    CommProtocol,
    /// The [comm.rs232.baud](https://www.zaber.com/protocol-manual#topic_setting_comm_rs232_baud) setting.
    CommRs232Baud,
    /// The [device.id](https://www.zaber.com/protocol-manual#topic_setting_device_id) setting.
    DeviceId,
    /// The [driver.current.hold](https://www.zaber.com/protocol-manual#topic_setting_driver_current_hold) setting.
    DriverCurrentHold,
    /// The [driver.current.max](https://www.zaber.com/protocol-manual#topic_setting_driver_current_max) setting.
    DriverCurrentMax,
    /// The [driver.current.run](https://www.zaber.com/protocol-manual#topic_setting_driver_current_run) setting.
    DriverCurrentRun,
    /// The [driver.current.servo](https://www.zaber.com/protocol-manual#topic_setting_driver_current_servo) setting.
    DriverCurrentServo,
    /// The [driver.dir](https://www.zaber.com/protocol-manual#topic_setting_driver_dir) setting.
    DriverDir,
    /// The [driver.enabled](https://www.zaber.com/protocol-manual#topic_setting_driver_enabled) setting.
    DriverEnabled,
    /// The [driver.temperature](https://www.zaber.com/protocol-manual#topic_setting_driver_temperature) setting.
    DriverTemperature,
    /// The [encoder.count](https://www.zaber.com/protocol-manual#topic_setting_encoder_count) setting.
    EncoderCount,
    /// The [encoder.count.cal](https://www.zaber.com/protocol-manual#topic_setting_encoder_count_cal) setting.
    EncoderCountCal,
    /// The [encoder.dir](https://www.zaber.com/protocol-manual#topic_setting_encoder_dir) setting.
    EncoderDir,
    /// The [encoder.pos](https://www.zaber.com/protocol-manual#topic_setting_encoder_pos) setting.
    EncoderPos,
    /// The [encoder.pos.error](https://www.zaber.com/protocol-manual#topic_setting_encoder_pos_error) setting.
    EncoderPosError,
    /// The [filter.holderid](https://www.zaber.com/protocol-manual#topic_setting_filter_holderid) setting.
    FilterHolderid,
    /// The [knob.dir](https://www.zaber.com/protocol-manual#topic_setting_knob_dir) setting.
    KnobDir,
    /// The [knob.distance](https://www.zaber.com/protocol-manual#topic_setting_knob_distance) setting.
    KnobDistance,
    /// The [knob.enable](https://www.zaber.com/protocol-manual#topic_setting_knob_enable) setting.
    KnobEnable,
    /// The [knob.maxspeed](https://www.zaber.com/protocol-manual#topic_setting_knob_maxspeed) setting.
    KnobMaxspeed,
    /// The [knob.mode](https://www.zaber.com/protocol-manual#topic_setting_knob_mode) setting.
    KnobMode,
    /// The [knob.speedprofile](https://www.zaber.com/protocol-manual#topic_setting_knob_speedprofile) setting.
    KnobSpeedprofile,
    /// The [lamp.current](https://www.zaber.com/protocol-manual#topic_setting_lamp_current) setting.
    LampCurrent,
    /// The [lamp.current.max](https://www.zaber.com/protocol-manual#topic_setting_lamp_current_max) setting.
    LampCurrentMax,
    /// The [lamp.flux](https://www.zaber.com/protocol-manual#topic_setting_lamp_flux) setting.
    LampFlux,
    /// The [lamp.flux.max](https://www.zaber.com/protocol-manual#topic_setting_lamp_flux_max) setting.
    LampFluxMax,
    /// The [lamp.status](https://www.zaber.com/protocol-manual#topic_setting_lamp_status) setting.
    LampStatus,
    /// The [lamp.temperature](https://www.zaber.com/protocol-manual#topic_setting_lamp_temperature) setting.
    LampTemperature,
    /// The [lamp.wavelength.fwhm](https://www.zaber.com/protocol-manual#topic_setting_lamp_wavelength_fwhm) setting.
    LampWavelengthFwhm,
    /// The [lamp.wavelength.peak](https://www.zaber.com/protocol-manual#topic_setting_lamp_wavelength_peak) setting.
    LampWavelengthPeak,
    /// The [limit.approach.maxspeed](https://www.zaber.com/protocol-manual#topic_setting_limit_approach_maxspeed) setting.
    LimitApproachMaxspeed,
    /// The [limit.away.action](https://www.zaber.com/protocol-manual#topic_setting_limit_away_action) setting.
    LimitAwayAction,
    /// The [limit.away.posupdate](https://www.zaber.com/protocol-manual#topic_setting_limit_away_posupdate) setting.
    LimitAwayPosupdate,
    /// The [limit.away.preset](https://www.zaber.com/protocol-manual#topic_setting_limit_away_preset) setting.
    LimitAwayPreset,
    /// The [limit.away.state](https://www.zaber.com/protocol-manual#topic_setting_limit_away_state) setting.
    LimitAwayState,
    /// The [limit.away.triggered](https://www.zaber.com/protocol-manual#topic_setting_limit_away_triggered) setting.
    LimitAwayTriggered,
    /// The [limit.c.action](https://www.zaber.com/protocol-manual#topic_setting_limit_c_action) setting.
    LimitCAction,
    /// The [limit.c.pos](https://www.zaber.com/protocol-manual#topic_setting_limit_c_pos) setting.
    LimitCPos,
    /// The [limit.c.posupdate](https://www.zaber.com/protocol-manual#topic_setting_limit_c_posupdate) setting.
    LimitCPosupdate,
    /// The [limit.c.preset](https://www.zaber.com/protocol-manual#topic_setting_limit_c_preset) setting.
    LimitCPreset,
    /// The [limit.c.state](https://www.zaber.com/protocol-manual#topic_setting_limit_c_state) setting.
    LimitCState,
    /// The [limit.c.triggered](https://www.zaber.com/protocol-manual#topic_setting_limit_c_triggered) setting.
    LimitCTriggered,
    /// The [limit.cycle.dist](https://www.zaber.com/protocol-manual#topic_setting_limit_cycle_dist) setting.
    LimitCycleDist,
    /// The [limit.detect.decelonly](https://www.zaber.com/protocol-manual#topic_setting_limit_detect_decelonly) setting.
    LimitDetectDecelonly,
    /// The [limit.detect.maxspeed](https://www.zaber.com/protocol-manual#topic_setting_limit_detect_maxspeed) setting.
    LimitDetectMaxspeed,
    /// The [limit.home.action](https://www.zaber.com/protocol-manual#topic_setting_limit_home_action) setting.
    LimitHomeAction,
    /// The [limit.home.posupdate](https://www.zaber.com/protocol-manual#topic_setting_limit_home_posupdate) setting.
    LimitHomePosupdate,
    /// The [limit.home.preset](https://www.zaber.com/protocol-manual#topic_setting_limit_home_preset) setting.
    LimitHomePreset,
    /// The [limit.home.state](https://www.zaber.com/protocol-manual#topic_setting_limit_home_state) setting.
    LimitHomeState,
    /// The [limit.home.triggered](https://www.zaber.com/protocol-manual#topic_setting_limit_home_triggered) setting.
    LimitHomeTriggered,
    /// The [limit.max](https://www.zaber.com/protocol-manual#topic_setting_limit_max) setting.
    LimitMax,
    /// The [limit.min](https://www.zaber.com/protocol-manual#topic_setting_limit_min) setting.
    LimitMin,
    /// The [limit.start.pos](https://www.zaber.com/protocol-manual#topic_setting_limit_start_pos) setting.
    LimitStartPos,
    /// The [lockstep.numgroups](https://www.zaber.com/protocol-manual#topic_setting_lockstep_numgroups) setting.
    LockstepNumgroups,
    /// The [maxspeed](https://www.zaber.com/protocol-manual#topic_setting_maxspeed) setting.
    Maxspeed,
    /// The [motion.accelonly](https://www.zaber.com/protocol-manual#topic_setting_motion_accelonly) setting.
    MotionAccelonly,
    /// The [motion.busy](https://www.zaber.com/protocol-manual#topic_setting_motion_busy) setting.
    MotionBusy,
    /// The [motion.decelonly](https://www.zaber.com/protocol-manual#topic_setting_motion_decelonly) setting.
    MotionDecelonly,
    /// The [motion.index.dist](https://www.zaber.com/protocol-manual#topic_setting_motion_index_dist) setting.
    MotionIndexDist,
    /// The [motion.index.num](https://www.zaber.com/protocol-manual#topic_setting_motion_index_num) setting.
    MotionIndexNum,
    /// The [motor.current.max](https://www.zaber.com/protocol-manual#topic_setting_motor_current_max) setting.
    MotorCurrentMax,
    /// The [parking.state](https://www.zaber.com/protocol-manual#topic_setting_parking_state) setting.
    ParkingState,
    /// The [peripheral.id](https://www.zaber.com/protocol-manual#topic_setting_peripheral_id) setting.
    PeripheralId,
    /// The [peripheral.id.pending](https://www.zaber.com/protocol-manual#topic_setting_peripheral_id_pending) setting.
    PeripheralIdPending,
    /// The [peripheral.serial](https://www.zaber.com/protocol-manual#topic_setting_peripheral_serial) setting.
    PeripheralSerial,
    /// The [peripheral.serial.pending](https://www.zaber.com/protocol-manual#topic_setting_peripheral_serial_pending) setting.
    PeripheralSerialPending,
    /// The [pos](https://www.zaber.com/protocol-manual#topic_setting_pos) setting.
    Pos,
    /// The [resolution](https://www.zaber.com/protocol-manual#topic_setting_resolution) setting.
    Resolution,
    /// The [scope.delay](https://www.zaber.com/protocol-manual#topic_setting_scope_delay) setting.
    ScopeDelay,
    /// The [scope.timebase](https://www.zaber.com/protocol-manual#topic_setting_scope_timebase) setting.
    ScopeTimebase,
    /// The [stream.numbufs](https://www.zaber.com/protocol-manual#topic_setting_stream_numbufs) setting.
    StreamNumbufs,
    /// The [stream.numstreams](https://www.zaber.com/protocol-manual#topic_setting_stream_numstreams) setting.
    StreamNumstreams,
    /// The [system.access](https://www.zaber.com/protocol-manual#topic_setting_system_access) setting.
    SystemAccess,
    /// The [system.axiscount](https://www.zaber.com/protocol-manual#topic_setting_system_axiscount) setting.
    SystemAxiscount,
    /// The [system.led.enable](https://www.zaber.com/protocol-manual#topic_setting_system_led_enable) setting.
    SystemLedEnable,
    /// The [system.serial](https://www.zaber.com/protocol-manual#topic_setting_system_serial) setting.
    SystemSerial,
    /// The [system.temperature](https://www.zaber.com/protocol-manual#topic_setting_system_temperature) setting.
    SystemTemperature,
    /// The [system.voltage](https://www.zaber.com/protocol-manual#topic_setting_system_voltage) setting.
    SystemVoltage,
    /// The [trigger.numactions](https://www.zaber.com/protocol-manual#topic_setting_trigger_numactions) setting.
    TriggerNumactions,
    /// The [trigger.numtriggers](https://www.zaber.com/protocol-manual#topic_setting_trigger_numtriggers) setting.
    TriggerNumtriggers,
    /// The [version](https://www.zaber.com/protocol-manual#topic_setting_version) setting.
    Version,
    /// The [version.build](https://www.zaber.com/protocol-manual#topic_setting_version_build) setting.
    VersionBuild,
}
}
