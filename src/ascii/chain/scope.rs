//! Types and traits for marking the scope of a type (i.e. whether an item is axis-scope or device-scope).

/// A marker trait indicating that a type represents an axis-scope item.
pub trait AxisScope {}
impl<T> AxisScope for &T where T: AxisScope {}
impl<T> AxisScope for &mut T where T: AxisScope {}

/// A marker trait indicating that a type represents a device-scope item.
pub trait DeviceScope {}
impl<T> DeviceScope for &T where T: DeviceScope {}
impl<T> DeviceScope for &mut T where T: DeviceScope {}

/// A concrete marker type for indicate that a wrapping type is axis-scope.
#[derive(Debug)]
pub enum AxisScopeMarker {}
impl AxisScope for AxisScopeMarker {}

/// A concrete marker type for indicate that a wrapping type is device-scope.
#[derive(Debug)]
pub enum DeviceScopeMarker {}
impl DeviceScope for DeviceScopeMarker {}

/// A marker trait indicating a type marker has the same scope as some other type `T`.
///
/// This should be used as a bounds on one of the marker types `AxisScopeMarker`
/// or `DeviceScopeMarker`.
///
/// For the inverse, see [`SameScopeAsMarker`].
pub trait MarksScopeOf<T> {}

impl<T> MarksScopeOf<T> for AxisScopeMarker where T: AxisScope {}
impl<T> MarksScopeOf<T> for DeviceScopeMarker where T: DeviceScope {}

/// A marker trait indicating a type has the same scope as some marker type `T`.
///
/// This should be used as a bounds on a type other than `AxisScopeMarker` and `DeviceScopeMarker`.
///
/// For the inverse, see [`MarksScopeOf`].
pub trait SameScopeAsMarker<T> {}

impl<T> SameScopeAsMarker<AxisScopeMarker> for T where T: AxisScope {}
impl<T> SameScopeAsMarker<DeviceScopeMarker> for T where T: DeviceScope {}
