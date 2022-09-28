//! Binary commands.
//!
//! See the [Zaber Binary protocol](https://www.zaber.com/protocol-manual?protocol=Binary#topic_quick_command_reference) documentation for a detailed description of each.
//!
//! Each command is given a unique type, which, together with Rust's trait
//! system, help enforce proper message construction at compile time. For
//! convenience, each type is convertible and comparable to `u8`.
//!
//! For cases when the extra type safety is problematic (e.g., changing commands
//! at run time or storing different commands in a collection), the versions of
//! each command constant in [`untyped`] can be used. They all have the same
//! type, but lack the type safety.

use super::{IoStates, Status, Version};

/// Define the types and implement the necessary traits for all the Binary commands.
///
/// Each command defined as a brace-enclosed (`{...}`) item, which each contains:
///
/// 1. the numerical value of the command
/// 2. the name of the command as space-separated idents (dashes are not permitted)
/// 3. the optional types the command can take as argument declared in the format `TakesData<...>`
///   * omitting `TakesData<...>` entirely implies the command cannot be sent by the host (i.e., `TakesData` and `TakesNoData` are not implemented for it).
///   * specifying `TakesData<>` implies that the command can be sent, but it takes no data argument (i.e., `TakesNoData` is implemented for it).
///   * specifying `TakesData<t...>` implies that the command can be sent and must have one of the comma-separated types
///     as a data argument (i.e, `TakesData<t> ...` are implemented for it).
/// 4. Either
///   * `ReplyData<>`, indicating it is never received from the device (i.e., does not implement `ReplyData`), or
///   * `ReplyData<r>` where `r` is the type of the data received from the device for this command (i.e., implements `ReplyData<r>`).
/// 5. An optional `ElicitsSelf` or `ElicitsSetOrReturn`.
///   * Omitting both indicates that the command does not elicit any response (i.e., doesn't implement `ElicitsResponse`)
///   * `ElicitsSelf` indicates the command in the elicited response has the same value as the command that was sent.
///   * `ElicitsSetOrReturn` indicates the command in the elicited response has the save value as the set/return command
///     in the data field (this is a special case only for the `Return Setting` command).
/// 6. An optional `SetOrReturnCommand`, indicating that the command is either a set/return command that can be used with
///    the `Return Setting` command (i.e., implements `SetOrReturnCommand` for the command).
macro_rules! define_commands {
    // Main entry case.
    // It's main purpose is to take the words in the name and concatenate them
    // before generating all the types and traits for each command.
    (
        $(
            {
                $value:literal,
                $($name_word:ident)+,
                $( TakesData<$($t:ty)?> ,)?
                ReplyData<$($r:ty)?>
                $(,
                    $elicits:ident
                    $(, $set_or_return:ident )?
                )?
            }
        ),+
        $(,)?
    ) => {
        paste::paste!{
            define_commands!{@main
                $({
                    $value,
                    $($name_word)+,
                    [< $($name_word:camel)+ >],
                    $( TakesData<$($t)?> ,)?
                    ReplyData<$($r)?>
                    $(,
                        $elicits
                        $(, $set_or_return )?
                    )?
                }),+
            }
        }
    };

    // This is where the magic happens: create types, implement appropriate traits for each type, and constants for each command.
    (@main
        $(
            {
                $value:literal,
                $($name_word:ident)+,
                $name:ident,
                $( TakesData<$($t:ty)?>, )?
                ReplyData<$($r:ty)?>
                $(,
                    $elicits:ident
                    $(, $set_or_return:ident )?
                )?
            }
        ),+
    ) => {
        paste::paste! {
            $(
                define_commands!{ @constant $value, $($name_word)+, $name, $( TakesData<$($t)?> ,)? ReplyData<$($r)?> }
            )+

            /// Get the name of a command.
            ///
            /// Returns None if the command is not recognized.
            pub fn name<C: crate::binary::traits::Command>(command: C) -> Option<&'static str> {
                name_impl(command.command())
            }

            /// The non-generic logic broken out of [`name`] to help reduce the size of
            /// each monomorphized version of the function.
            fn name_impl(command: u8) -> Option<&'static str> {
                match command {
                    $(
                        $value => Some(stringify!($($name_word)+)),
                    )+
                    _ => None,
                }
            }

            pub mod untyped {
                //! Binary commands as `u8`s, which removes most of the type
                //! safety and inferencing available with the commands in the
                //! [`command`](super) module.
                //!
                //! However, because the commands all share the same type, they
                //! can be stored together in collections or swapped out with
                //! one another at run time.
                //!
                //! For more information see the [`binary`](crate::binary#type-safety) module
                //! documentation.

                $(
                    #[doc = "The Binary " $(" " $name_word " ")+ "(`" $value "`) command as a `u8`."]
                    pub const [< $name:snake:upper >] : u8 = $value;
                )+
            }

            pub mod types {
                //! The unique types for each command.
                //!
                //! These types are not publicly constructable, so it is recommended to use
                //! the constants defined in [`commands`](super), which are instances of
                //! these types.
                use crate::binary::traits::*;
                use super::*;
                $(
                    #[doc = "A type that encodes compile time information about the " $(" " $name_word " ")+ " (`" $value "`) command."]
                    #[doc = ""]
                    #[doc = "It is not publicly constructable. Use the [`" [< $name:snake:upper >] "`] constant."]
                    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
                    pub struct $name(pub(crate) ());
                    impl $name {
                        #[doc = "The `u8` value of the command (`" $value "`)."]
                        const VALUE: u8 = $value;
                    }
                    impl private::Sealed for $name {}
                    impl Command for $name {
                        fn command(&self) -> u8 {
                            u8::from(*self)
                        }
                    }
                    impl TryFrom<u8> for $name {
                        type Error = std::num::TryFromIntError;

                        fn try_from(value: u8) -> Result<Self, Self::Error> {
                            if Self::VALUE == value {
                                Ok($name(()))
                            } else {
                                Err(u8::try_from(-1).unwrap_err())
                            }
                        }
                    }
                    impl From<$name> for u8 {
                        fn from(_: $name) -> u8 {
                            $name::VALUE
                        }
                    }
                    impl PartialEq<$name> for u8
                    {
                        fn eq(&self, _: &$name) -> bool {
                            $name::VALUE == *self
                        }
                    }
                    impl PartialEq<u8> for $name
                    {
                        fn eq(&self, other: &u8) -> bool {
                            $name::VALUE == *other
                        }
                    }

                    $(define_commands!{@takes $name, $($t)? })?
                    $($(define_commands!{@set_or_return $name, $set_or_return })?)?
                    $(define_commands!{@elicits $name, $elicits })?
                    $(impl ReplyData<$r> for $name {})?
                )+
            }

            use crate::binary::traits;

            // Allow any "untyped" command to elicit a response. However, the
            // what the expected response should be must be determined at runtime
            // since the type cannot encode all that information.
            impl<D> traits::ElicitsResponse for (u8, u8, D)
            where
                D: traits::Data + Copy,
                u8: TryFrom<D>,
            {
                type Response = u8;

                /// Calculate the expected response for a message at runtime.
                fn expected_command(&self) -> traits::ExpectedCommandResult<Self::Response> {
                    match self.1 {
                        $(
                            $value => define_commands!(@elicits_u8_logic self, $($elicits)?)
                        ),+
                        ,
                        // An unknown command was sent. This could be a new
                        // command that this library knows nothing about. For
                        // ease of use, accept any response rather than rejecting.
                        _ => traits::ExpectedCommandResult::AnyAcceptable,
                    }
                }
            }

            // Although (u8, u8) does not implement TxMessage, implement
            // ElicitsResponse for it to avoid hiding compiler error messages
            // about incorrect message structure with misleading errors about
            // the type not implementing ElicitsResponse.
            impl traits::ElicitsResponse for (u8, u8) {
                type Response = u8;
                fn expected_command(&self) -> traits::ExpectedCommandResult<Self::Response> {
                    // This should never be possible because (u8, u8) does not
                    // implement TxMessage. u8 based messages require data field.
                    unreachable!();
                }
            }
        }
    };

    // Define the constant for a command that takes no data and cannot be sent from the host.
    (@constant $value:literal, $($name_word:ident)+, $name:ident, ReplyData<$($r:ty)?>) => {
        paste::paste! {
            #[doc = "The Binary " $(" " $name_word " ")+ "(`" $value "`) command."]
            #[doc = ""]
            #[doc = "This command cannot be transmitted by the host, only received from a device."]
            $( #[doc = "When received, the data field has the type [`" $r "`]."] )?
            pub const [< $name:snake:upper >] : types::$name = types::$name(());
        }
    };
    // Define the constant for a command that takes not data but can be sent from the host.
    (@constant $value:literal, $($name_word:ident)+, $name:ident, TakesData<>, ReplyData<$($r:ty)?>) => {
        paste::paste! {
            #[doc = "The Binary " $(" " $name_word " ")+ "(`" $value "`) command."]
            #[doc = ""]
            #[doc = "When transmitted by the host this command does not take any data argument."]
            $( #[doc = "When received, the data field has the type [`" $r "`]."] )?
            pub const [< $name:snake:upper >] : types::$name = types::$name(());
        }
    };
    // Define the constant for a command that takes data and can be sent from the host.
    (@constant $value:literal, $($name_word:ident)+, $name:ident, TakesData<$t:ty>, ReplyData<$($r:ty)?>) => {
        paste::paste! {
            #[doc = "The Binary " $(" " $name_word " ")+ "(`" $value "`) command."]
            #[doc = ""]
            #[doc = "When transmitted by the host this command requires data of the type [`" $t "`]." ]
            $( #[doc = "When received, the data field has the type [`" $r "`]."] )?
            pub const [< $name:snake:upper >] : types::$name = types::$name(());
        }
    };

    // Define the TakesNoData trait for commands that do not take any data.
    (@takes $name:ident,) => {
        impl TakesNoData for $name {}
    };
    // Define the TakesData traits for commands that do take date.
    (@takes $name:ident, $t:ty) => {
        impl TakesData<$t> for $name {}
    };

    // Mark the command as a set/return command
    (@set_or_return $name:ident, SetOrReturnCommand ) => {
        impl SetOrReturnCommand for $name {}
        // SetOrReturnCommand requires the Command trait (which is already
        // implemented in @main) and the Data trait, so implement that here.
        impl Data for $name {
            type Error = std::num::TryFromIntError;

            fn fill_data(&self, buffer: &mut [u8]) {
                $name::VALUE.fill_data(buffer);
            }
            fn try_from_data(buffer: [u8; 4]) -> Result<Self, Self::Error> {
                Self::try_from(u8::try_from(i32::from_le_bytes(buffer))?)
            }
        }
        impl TakesData<$name> for ReturnSetting {}
    };

    // The command elicits a response with the same command type as itself.
    //
    // Although the type either takes data or not, implement ElicitsResponse
    // for regardless of whether it has data. That logic is encoded via the
    // TxMessage trait. If the TakesData or TakesNoData trait bounds were also
    // added here it would cause compiler errors about types not implementing
    // ElicitsResponse when in actual fact the problem is an incorrectly missing
    // or present data field.
    (@elicits $name:ident, ElicitsSelf) => {
        impl ElicitsResponse for (u8, $name) {
            type Response = $name;
            fn expected_command(&self) -> ExpectedCommandResult<Self::Response> {
                ExpectedCommandResult::Exactly($name(()))
            }
        }
        impl<D> ElicitsResponse for (u8, $name, D) {
            type Response = $name;
            fn expected_command(&self) -> ExpectedCommandResult<Self::Response> {
                ExpectedCommandResult::Exactly($name(()))
            }
        }
    };
    // The command elicits a response that matches its data arguments type,
    // which must be a set or return command.
    (@elicits $name:ident, ElicitsSetOrReturn) => {
        impl<T> ElicitsResponse for (u8, $name, T)
        where
            T: Command
        {
            type Response = T;
            fn expected_command(&self) -> ExpectedCommandResult<Self::Response> {
                ExpectedCommandResult::Exactly(self.2)
            }
        }
    };

    // The message does not elicit any response, so anything we receive should be unexpected
    (@elicits_u8_logic $message:ident, ) => { traits::ExpectedCommandResult::AnyUnexpected };
    // The message elicits a response with the same command as itself.
    (@elicits_u8_logic $message:ident, ElicitsSelf) => { traits::ExpectedCommandResult::Exactly($message.1) };
    // The message elicits a response with a command that matches its data value.
    (@elicits_u8_logic $message:ident, ElicitsSetOrReturn) => {
        if let Ok(value) = u8::try_from($message.2) {
            traits::ExpectedCommandResult::Exactly(value)
        } else {
            // The data value is not a valid command. Firmware should reply with
            // an Error command, meaning we should never get here.
            traits::ExpectedCommandResult::AnyUnexpected
        }
    };
}

// Define types and associated traits for each binary command.
//
// When defining the types for argument and return data, unless a Rust data type
// exactly matches the bounds for the command and the semantic meaning, `i32`
// should be used. This prevents needless type conversions that don't actually
// improve type safety or remove confusion. For instance, `bool` should only be
// used for data that not only have just 2 values but for which one has a
// "truthiness" (e.g., settings that enable something).
define_commands! {
    { 0, Reset, TakesData<>, ReplyData<> },
    { 1, Home, TakesData<>, ReplyData<i32>, ElicitsSelf },
    { 2, Renumber, TakesData<u8>, ReplyData<i32>, ElicitsSelf }, // Responds from new device number
    { 8, Move Tracking, ReplyData<i32> },
    { 9, Limit Active, ReplyData<i32> },
    { 10, Manual Move Tracking, ReplyData<i32> },
    { 11, Manual Move, ReplyData<i32> },
    { 12, Slip Tracking, ReplyData<i32> },
    { 13, Unexpected Position, ReplyData<i32> },
    { 16, Store Current Position, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 17, Return Stored Position, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 18, Move to Stored Position, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 20, Move Absolute, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 21, Move Relative, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 22, Move at Constant Speed, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 23, Stop, TakesData<>, ReplyData<i32>, ElicitsSelf },
    { 36, Restore Settings, TakesData<i32>, ReplyData<i32>, ElicitsSelf},
    { 37, Set Microstep Resolution, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 38, Set Run Current, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 39, Set Hold Current, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 40, Set Device Mode, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 41, Set Home Speed, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 42, Set Target Speed, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 43, Set Acceleration, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 44, Set Maximum Position, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 45, Set Current Position, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 47, Set Home Offset, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 48, Set Alias Number, TakesData<u8>, ReplyData<u8>, ElicitsSelf, SetOrReturnCommand },
    { 50, Return Device Id, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 51, Return Firmware Version, TakesData<>, ReplyData<Version>, ElicitsSelf, SetOrReturnCommand },
    { 52, Return Power Supply Voltage, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 53, Return Setting, TakesData<u8>, ReplyData<u8>, ElicitsSetOrReturn },
    { 54, Return Status, TakesData<>, ReplyData<Status>, ElicitsSelf, SetOrReturnCommand },
    { 55, Echo Data, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 56, Return Firmware Build, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 60, Return Current Position, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 63, Return Serial Number, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 65, Set Park State, TakesData<bool>, ReplyData<bool>, ElicitsSelf, SetOrReturnCommand },
    { 66, Set Peripheral Id, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 67, Return Digital Input Count, TakesData<>, ReplyData<i32>, ElicitsSelf },
    { 68, Read Digital Input, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 69, Read All Digital Inputs, TakesData<>, ReplyData<IoStates>, ElicitsSelf },
    { 70, Return Digital Output Count, TakesData<>, ReplyData<i32>, ElicitsSelf },
    { 71, Read Digital Output, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 72, Read All Digital Outputs, TakesData<>, ReplyData<IoStates>, ElicitsSelf },
    { 73, Write Digital Output, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 74, Write All Digital Outputs, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 75, Return Analog Input Count, TakesData<>, ReplyData<i32>, ElicitsSelf },
    { 76, Read Analog Input, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 78, Move Index, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 79, Set Index Distance, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 80, Set Cycle Distance, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 81, Set Filter Holder Id, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 82, Return Encoder Count, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 83, Return Calibrated Encoder Count, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 84, Return Calibration Type, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 85, Return Calibration Error, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 86, Return Peripheral Serial Number, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 89, Return Encoder Position, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 91, Return Pending Peripheral Id, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 92, Return Pending Peripheral Serial Number, TakesData<>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 93, Activate, TakesData<>, ReplyData<>, ElicitsSelf },
    { 101, Set Auto Reply Disabled Mode, TakesData<bool>, ReplyData<bool>, ElicitsSelf, SetOrReturnCommand },
    { 102, Set Message Id Mode, TakesData<bool>, ReplyData<bool>, ElicitsSelf, SetOrReturnCommand },
    { 103, Set Home Status, TakesData<bool>, ReplyData<bool>, ElicitsSelf, SetOrReturnCommand },
    { 104, Set Home Sensor Type, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 105, Set Auto Home Disabled Mode, TakesData<bool>, ReplyData<bool>, ElicitsSelf, SetOrReturnCommand },
    { 106, Set Minimum Position, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 107, Set Knob Disabled Mode, TakesData<bool>, ReplyData<bool>, ElicitsSelf, SetOrReturnCommand },
    { 108, Set Knob Direction, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 109, Set Knob Movement Mode, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 110, Set Knob Jog Size, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 111, Set Knob Velocity Scale, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 112, Set Knob Velocity Profile, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 113, Set Acceleration Only, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 114, Set Deceleration Only, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 115, Set Move Tracking Mode, TakesData<bool>, ReplyData<bool>, ElicitsSelf, SetOrReturnCommand },
    { 116, Set Manual Move Tracking Disabled Mode, TakesData<bool>, ReplyData<bool>, ElicitsSelf, SetOrReturnCommand },
    { 117, Set Move Tracking Period, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 118, Set Closed Loop Mode, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 119, Set Slip Tracking Period, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 120, Set Stall Timeout, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 122, Set Baud Rate, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 123, Set Protocol, TakesData<i32>, ReplyData<i32>, ElicitsSelf, SetOrReturnCommand },
    { 124, Convert to Ascii, TakesData<i32>, ReplyData<i32>, ElicitsSelf },
    { 255, Error, ReplyData<i32> },
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn command_names() {
        assert_eq!(name(RESET), Some("Reset"));
        assert_eq!(name(SET_TARGET_SPEED), Some("Set Target Speed"));
        assert_eq!(
            name(SET_AUTO_REPLY_DISABLED_MODE),
            Some("Set Auto Reply Disabled Mode")
        );
        assert_eq!(name(CONVERT_TO_ASCII), Some("Convert to Ascii"));
        assert_eq!(name(254), None);
    }
}
