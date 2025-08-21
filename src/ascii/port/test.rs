use std::cell::Cell;

use crate::{
	ascii::{
		response::{
			check::{self, unchecked},
			Alert, AnyResponse, Info, Reply,
		},
		Port,
	},
	backend::Mock,
	error::*,
};

/// Generate code to check the behaviour of different port methods.
///
/// The syntax is `<port>, <case>...` where multiple `<case>`s are separated
/// by `,` and can be of two flavors:
///
///   * `ok case <response_bytes_to_append>... via <closure_to_generate_responses>`
///   * `err case <response_bytes_to_append>... via <closure_to_generate_responses> => <expected_error_type>`
macro_rules! check_cases {
    (
        $port:ident, ok case $($response_bytes:literal),+ via $method:expr, $($rest:tt)*
    ) => {
        // Make sure there are no other responses left over from other test cases
        $port.backend.clear();
        $(
            $port.backend.push($response_bytes);
        )+
        let m: fn(&mut Port<'_, _, _>) -> Result<_, _> = $method; // Give the compiler the necessary type hints
        match (m)(&mut $port) {
            Err(e) => panic!("unexpected error when reading {} via {}:\n\tactual error: {}\n\t{:?}\n",
                stringify!($($response_bytes),+),
                stringify!($method),
                e,
                e),
            _ => {},
        }
        check_cases!($port, $($rest)*)
    };

    (
        $port:ident, err case $($response_bytes:literal),+ via $method:expr => $err_type:ident, $($rest:tt)*
    ) => {
        $port.backend.clear();
        $(
            $port.backend.push($response_bytes);
        )+
        let m: fn(&mut Port<'_, _, _>) -> Result<_, _> = $method; // Give the compiler the necessary type hints
        match (m)(&mut $port) {
            Err(e) => {
                if let Err(e) = $err_type::try_from(e) {
                    panic!("unexpected error when reading {} via {}:\n\texpected:\t{}\n\tgot:\t\t{}\n\t\t\t{:?}\n",
                        stringify!($($response_bytes),+),
                        stringify!($method),
                        stringify!($err_type),
                        e,
                        e);
                }
            }
            Ok(_) => panic!("unexpected Ok when reading {} via {}", stringify!($method), stringify!($($response_bytes)+)),
        };
        check_cases!($port, $($rest)*)
    };

    ($port:ident, ) => {};
}

#[test]
fn command_reply_ok() {
	let mut port = Port::open_mock();
	port.backend.push(b"@01 0 OK IDLE -- 0\r\n");
	let reply = port.command_reply("").unwrap().flag_ok().unwrap();
	assert_eq!(reply.target(), (1, 0).into());

	// Multi-packet Reply
	{
		let backend = &mut port.backend;
		backend.push(b"@01 0 OK IDLE -- part1\\\r\n");
		backend.push(b"#01 0 cont part2a part2b\\\r\n");
		backend.push(b"#01 0 cont part3\r\n");
	}
	let reply = port.command_reply("").unwrap().flag_ok().unwrap();
	assert_eq!(reply.data(), "part1 part2a part2b part3");
}

#[test]
fn command_reply_fail() {
	let mut port = Port::open_mock();

	// Incorrect kind
	port.backend.push(b"!01 0 IDLE FF 0\r\n");
	let err = port.command_reply("").unwrap_err();
	assert!(matches!(err, AsciiError::UnexpectedResponse(_)), "{err:?}");

	// Incorrect target
	port.backend.push(b"!02 0 IDLE FF 0\r\n");
	let err = port.command_reply((1, "")).unwrap_err();
	assert!(matches!(err, AsciiError::UnexpectedResponse(_)));

	// Unexpected Alert interleaved in reply packets.
	{
		let backend = &mut port.backend;
		backend.push(b"@01 0 OK IDLE -- part1\\\r\n");
		backend.push(b"!02 0 IDLE -- 0\r\n");
		backend.push(b"#01 0 cont part2\r\n");
	}
	let err = port.command_reply((1, "")).unwrap_err();
	assert!(matches!(err, AsciiError::UnexpectedResponse(_)));

	// Unexpected and incomplete Alert interleaved in reply packets.
	{
		let backend = &mut port.backend;
		backend.push(b"@01 0 OK IDLE -- part1\\\r\n");
		backend.push(b"!02 0 IDLE -- something\\\r\n");
		backend.push(b"#01 0 cont part2\r\n");
	}
	let err = port.command_reply((1, "")).unwrap_err();
	assert!(matches!(err, AsciiError::UnexpectedPacket(_)));
}

#[test]
fn command_reply_unexpected_alert() {
	let alert_count = Cell::new(0);

	let mut port = Port::open_mock();
	port.set_unexpected_alert_handler(|_alert| {
		alert_count.set(alert_count.get() + 1);
		Ok(()) // Consume any alert
	});

	port.backend.push(b"!01 0 IDLE --\r\n");
	port.backend.push(b"@01 0 OK IDLE -- 0\r\n");
	let reply = port.command_reply("").unwrap().flag_ok().unwrap();
	assert_eq!(reply.target(), (1, 0).into());
	assert_eq!(alert_count.get(), 1);

	// Multi-packet Reply
	{
		let backend = &mut port.backend;
		backend.push(b"@01 0 OK IDLE -- part1\\\r\n");
		backend.push(b"#01 0 cont part2a part2b\\\r\n");
		backend.push(b"!02 1 IDLE --\r\n");
		backend.push(b"!02 1 IDLE --\r\n");
		backend.push(b"#01 0 cont part3\r\n");
	}
	let reply = port.command_reply("").unwrap().flag_ok().unwrap();
	assert_eq!(reply.data(), "part1 part2a part2b part3");
	assert_eq!(alert_count.get(), 3);
}

#[test]
fn command_reply_n_ok() {
	let mut port = Port::open_mock();
	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 0\r\n");
		buf.push(b"@02 0 OK IDLE -- 0\r\n");
	}
	let _ = port.command_reply_n("", 2, check::strict()).unwrap();

	// Interleaved multi-packet replies
	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 1part1\\\r\n");
		buf.push(b"@02 0 OK IDLE -- 2part1\\\r\n");
		buf.push(b"#02 0 cont 2part2\r\n");
		buf.push(b"#01 0 cont 1part2\r\n");
	}
	let replies = port.command_reply_n("", 2, check::strict()).unwrap();
	let reply_data: Vec<_> = replies.iter().map(Reply::data).collect();
	assert_eq!(reply_data, &["1part1 1part2", "2part1 2part2"]);
}

#[test]
fn command_reply_n_fail() {
	let mut port = Port::open_mock();

	// Timeout waiting for non-existent message.
	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 0\r\n");
		buf.push(b"@02 0 OK IDLE -- 0\r\n");
	}
	let err = port.command_reply_n("", 3, check::strict()).unwrap_err();
	assert!(err.is_timeout());

	// Timeout waiting for non-existent packet.
	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 0\\\r\n");
		buf.push(b"@02 0 OK IDLE -- 0\r\n");
	}
	let err = port.command_reply_n("", 2, check::strict()).unwrap_err();
	assert!(err.is_timeout());
}

#[test]
fn command_reply_n_unexpected_alert() {
	let alert_count = Cell::new(0);

	let mut port = Port::open_mock();
	port.set_unexpected_alert_handler(|_alert| {
		alert_count.set(alert_count.get() + 1);
		Ok(()) // Consume any alert
	});

	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 0\r\n");
		buf.push(b"!03 0 IDLE --\r\n");
		buf.push(b"@02 0 OK IDLE -- 0\r\n");
	}
	let _ = port.command_reply_n("", 2, check::strict()).unwrap();
	assert_eq!(alert_count.get(), 1);

	// Interleaved multi-packet replies
	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 1part1\\\r\n");
		buf.push(b"!03 0 IDLE --\r\n");
		buf.push(b"@02 0 OK IDLE -- 2part1\\\r\n");
		buf.push(b"!04 0 IDLE --\r\n");
		buf.push(b"#02 0 cont 2part2\r\n");
		buf.push(b"#01 0 cont 1part2\r\n");
		buf.push(b"!05 0 IDLE --\r\n"); // Shouldn't be read
	}
	let replies = port.command_reply_n("", 2, check::strict()).unwrap();
	let reply_data: Vec<_> = replies.iter().map(Reply::data).collect();
	assert_eq!(reply_data, &["1part1 1part2", "2part1 2part2"]);
	assert_eq!(alert_count.get(), 3);
}

#[test]
fn command_replies_until_timeout_ok() {
	let mut port = Port::open_mock();
	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 0\r\n");
		buf.push(b"@02 0 OK IDLE -- 0\r\n");
	}
	let replies = port
		.command_replies_until_timeout("", check::strict())
		.unwrap();
	assert_eq!(replies.len(), 2);
}

#[test]
fn command_replies_mixed_cont_until_timeout_ok() {
	let expected = &["part 1a part 1b", "part 2a part 2b"];

	let mut port = Port::open_mock();
	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- part 1a\\\r\n");
		buf.push(b"@02 0 OK IDLE -- part 2a\\\r\n");
		buf.push(b"#01 0 cont part 1b\r\n");
		buf.push(b"#02 0 cont part 2b\r\n");
	}
	let replies = port
		.command_replies_until_timeout("", check::strict())
		.unwrap();
	let reply_data: Vec<_> = replies.iter().map(Reply::data).collect();
	assert_eq!(reply_data, expected);

	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- part 1a\\\r\n");
		buf.push(b"@02 0 OK IDLE -- part 2a\\\r\n");
		buf.push(b"#02 0 cont part 2b\r\n");
		buf.push(b"#01 0 cont part 1b\r\n");
	}
	let replies = port
		.command_replies_until_timeout("", check::strict())
		.unwrap();
	let reply_data: Vec<_> = replies.iter().map(Reply::data).collect();
	// When the continuations come shouldn't change the response order.
	assert_eq!(reply_data, expected);

	{
		let buf = &mut port.backend;
		buf.push(b"@02 0 OK IDLE -- part 2a\\\r\n");
		buf.push(b"@01 0 OK IDLE -- part 1a\\\r\n");
		buf.push(b"#02 0 cont part 2b\r\n");
		buf.push(b"#01 0 cont part 1b\r\n");
	}
	let replies = port
		.command_replies_until_timeout("", check::strict())
		.unwrap();
	let reply_data: Vec<_> = replies.iter().map(Reply::data).collect();
	// The initial packet order should change the response order.
	assert_eq!(
		reply_data,
		expected.iter().copied().rev().collect::<Vec<_>>()
	);
}

#[test]
fn command_replies_until_timeout_fail() {
	let mut port = Port::open_mock();
	{
		let buf = &mut port.backend;
		buf.push(b"@01 1 OK IDLE -- 0\r\n");
		buf.push(b"@02 2 OK IDLE -- 0\r\n"); // Wrong axis number
		buf.push(b"!03 1 IDLE -- 0\r\n"); // Wrong kind
	}
	let err = port
		.command_replies_until_timeout(((0, 1), "get pos"), check::strict()) // To all first axes
		.unwrap_err();
	assert!(matches!(err, AsciiError::UnexpectedResponse(_)));
}

#[test]
fn command_replies_until_timeout_unexpected_alert() {
	let alert_count = Cell::new(0);

	let mut port = Port::open_mock();
	port.set_unexpected_alert_handler(|_alert| {
		alert_count.set(alert_count.get() + 1);
		Ok(()) // Consume any alert
	});

	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 0\r\n");
		buf.push(b"!03 0 IDLE --\r\n");
		buf.push(b"@02 0 OK IDLE -- 0\r\n");
		buf.push(b"!04 0 IDLE --\r\n");
	}
	let replies = port
		.command_replies_until_timeout("", check::strict())
		.unwrap();
	assert_eq!(replies.len(), 2);
	assert_eq!(alert_count.get(), 2);
}

#[test]
fn command_replies_mixed_cont_until_timeout_unexpected_alert() {
	let expected = &["part 1a part 1b", "part 2a part 2b"];

	let alert_count = Cell::new(0);

	let mut port = Port::open_mock();
	port.set_unexpected_alert_handler(|_alert| {
		alert_count.set(alert_count.get() + 1);
		Ok(()) // Consume any alert
	});

	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- part 1a\\\r\n");
		buf.push(b"!03 0 IDLE --\r\n");
		buf.push(b"@02 0 OK IDLE -- part 2a\\\r\n");
		buf.push(b"#01 0 cont part 1b\r\n");
		buf.push(b"!04 0 IDLE --\r\n");
		buf.push(b"#02 0 cont part 2b\r\n");
		buf.push(b"!05 0 IDLE --\r\n");
	}
	let replies = port
		.command_replies_until_timeout("", check::strict())
		.unwrap();
	let reply_data: Vec<_> = replies.iter().map(Reply::data).collect();
	assert_eq!(reply_data, expected);
	assert_eq!(alert_count.get(), 3);
}

#[test]
fn command_reply_infos_unexpected_alert() {
	let alert_count = Cell::new(0);

	let mut port = Port::open_mock();
	port.set_unexpected_alert_handler(|_alert| {
		alert_count.set(alert_count.get() + 1);
		Ok(()) // Consume any alert
	});

	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 0\r\n");
		buf.push(b"#01 0 foo\r\n");
		buf.push(b"#01 0 bar\r\n");
		buf.push(b"!03 0 IDLE --\r\n");
		buf.push(b"#01 1 baz\r\n");
		buf.push(b"@01 0 1 OK IDLE -- 0\r\n");
	}
	let (_reply, infos) = port.command_reply_infos("", check::strict()).unwrap();
	assert_eq!(infos.len(), 3);
	assert_eq!(alert_count.get(), 1);
}

#[test]
fn response_until_timeout_ok() {
	let mut port = Port::open_mock();

	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 0\r\n");
		buf.push(b"@02 0 OK IDLE -- 0\r\n");
	}
	let replies: Vec<AnyResponse> = port.responses_until_timeout(check::strict()).unwrap();
	let reply_data: Vec<_> = replies.iter().map(AnyResponse::data).collect();
	assert_eq!(reply_data, &["0", "0"]);

	// Multi-packet info messages
	{
		let buf = &mut port.backend;
		buf.push(b"#01 0 part 1a\\\r\n");
		buf.push(b"#02 0 part 2a\\\r\n");
		buf.push(b"#01 0 cont part 1b\r\n");
		buf.push(b"#02 0 cont part 2b\r\n");
	}
	let replies: Vec<AnyResponse> = port.responses_until_timeout(check::strict()).unwrap();
	let reply_data: Vec<_> = replies.iter().map(AnyResponse::data).collect();
	assert_eq!(reply_data, &["part 1a part 1b", "part 2a part 2b"]);
}

#[test]
fn response_until_timeout_fail() {
	let mut port = Port::open_mock();

	// Received an alert part way should not read following messages.
	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 0\r\n");
		buf.push(b"!02 1 IDLE -- \r\n");
		buf.push(b"@02 0 OK IDLE -- 0\r\n");
	}
	let err = port
		.responses_until_timeout::<Reply, _>(check::strict())
		.unwrap_err();
	assert!(matches!(err, AsciiError::UnexpectedResponse(_)));
	// Can read the final reply
	let _ = port.response::<Reply>().unwrap();

	// Invalid continuation packet
	{
		let buf = &mut port.backend;
		buf.push(b"@01 0 OK IDLE -- 0\r\n");
		buf.push(b"#01 0 cont something\r\n");
	}
	let err = port
		.responses_until_timeout::<Reply, _>(check::strict())
		.unwrap_err();
	assert!(matches!(err, AsciiError::UnexpectedPacket(_)));
}

/// Ensure that explicitly reading an alert message while an `set_unexpected_alert_handler`
/// callback is configured, does not trigger the callback.
#[test]
fn explicit_alert_response_does_not_trigger_unexpected_alert_callback() {
	use crate::ascii::response::Alert;

	let alert_count = Cell::new(0);

	let mut port = Port::open_mock();
	port.set_unexpected_alert_handler(|_alert| {
		alert_count.set(alert_count.get() + 1);
		Ok(()) // Consume any alert
	});

	{
		let buf = &mut port.backend;
		buf.push(b"!01 0 IDLE --\r\n");
	}
	let _ = port.response::<Alert>().unwrap();
	assert_eq!(alert_count.get(), 0);
}

/// Ensure that setting explicit types is possible for all. This
/// inferencing was previously forbidden because `arg: impl Bound`
/// syntax was used. Replacing that syntax with standard `where` bounds
/// allows for the explicit type.
#[test]
fn type_inference_regression_test() {
	use super::check::strict;

	let mut port = Port::open_mock();
	let _ = port.response::<AnyResponse>();
	let _ = port.response_n::<AnyResponse, _>(2, strict());
	let _ = port.responses_until_timeout::<AnyResponse, _>(strict());
}

#[test]
fn set_message_ids() {
	let mut port = Port::open_mock();
	assert!(!port.message_ids());
	assert!(!port.set_message_ids(true));
	assert!(port.message_ids());
	assert!(port.set_message_ids(true));
	assert!(port.message_ids());
}

#[test]
fn set_checksums() {
	let mut port = Port::open_mock();
	assert!(!port.checksums());
	assert!(!port.set_checksums(true));
	assert!(port.checksums());
	assert!(port.set_checksums(true));
	assert!(port.checksums());
}

#[test]
fn set_max_packet_size() {
	use super::MaxPacketSize;

	let custom = MaxPacketSize::new(81).unwrap();
	let default = MaxPacketSize::default();

	let mut port = Port::open_mock();
	assert_eq!(port.max_packet_size(), default);
	assert_eq!(port.set_max_packet_size(custom), default);
	assert_eq!(port.max_packet_size(), custom);
}

#[test]
fn reject_reserved_characters() {
	let mut port = Port::open_mock();

	let reserved_characters = b"/@#!:\\\r\n".iter().copied().chain(128u8..=u8::MAX);
	for reserved in reserved_characters {
		let err = port.command([reserved]).unwrap_err();
		assert!(matches!(err, AsciiError::ReservedCharacter(_)));
	}
}

#[test]
fn read_packet_bytes() {
	struct Case<'a> {
		input: &'a [&'a [u8]],
		expected: Result<&'a [u8], AsciiError>,
	}
	let test_cases: &[Case<'_>] = &[
		Case {
			input: &[b"/\r\n"],
			expected: Ok(b"/\r\n"),
		},
		Case {
			input: &[b"/\n"],
			expected: Ok(b"/\n"),
		},
		Case {
			input: &[b"/\r"],
			expected: Err(AsciiPacketMissingEndError::new(b"/\r").into()),
		},
		Case {
			input: &[b"!\r\n"],
			expected: Ok(b"!\r\n"),
		},
		Case {
			input: &[b"#\r\n"],
			expected: Ok(b"#\r\n"),
		},
		Case {
			input: &[b"@\r\n"],
			expected: Ok(b"@\r\n"),
		},
		Case {
			input: &[b"\0\t\r@01 1 OK IDLE --\r\n"],
			expected: Ok(b"@01 1 OK IDLE --\r\n"),
		},
		Case {
			input: &[b"  /1 1 tools echo / this\n"],
			expected: Err(AsciiPacketMissingEndError::new(b"/1 1 tools echo ").into()),
		},
		Case {
			input: &[b"  /1 1 tools echo\nextra"],
			expected: Ok(b"/1 1 tools echo\n"),
		},
		Case {
			input: &[b"\r\n"],
			expected: Err(AsciiPacketMissingStartError::new(b"").into()),
		},
		Case {
			input: &[b"/anything here"],
			expected: Err(AsciiPacketMissingEndError::new(b"/anything here").into()),
		},
	];
	let mut port = Port::open_mock();
	for case in test_cases {
		port.backend.clear();
		for packet in case.input {
			port.backend.push(packet);
		}
		let actual = port.read_packet_bytes();
		match &case.expected {
			Ok(expected) => {
				assert_eq!(*expected, actual.expect("expected OK"));
			}
			Err(AsciiError::PacketMissingStart(packet)) => {
				match &actual.expect_err("expected Err") {
					AsciiError::PacketMissingStart(actual) => assert_eq!(packet, actual),
					e => panic!("unexpected error: {e:?}"),
				}
			}
			Err(AsciiError::PacketMissingEnd(packet)) => match &actual.expect_err("expected Err") {
				AsciiError::PacketMissingEnd(actual) => assert_eq!(packet, actual),
				e => panic!("unexpected error: {e:?}"),
			},
			Err(_) => panic!("unsupported test case"),
		}
	}
}

#[test]
fn set_packet_handler() {
	use std::cell::RefCell;

	let responses = [
		b"@01 0 OK IDLE -- 0\r\n".as_ref(),
		b"!02 1 IDLE -- \r\n".as_ref(),
		b"@02 0 OK IDLE -- 0\r\n".as_ref(),
	];
	// `captured` must be declared before the port so that it is dropped after it.
	// This is necessary because the port holds a reference to `captured`
	// via the set_packet_handler closure.
	let captured = RefCell::new(Vec::new());
	let mut port = Port::open_mock();
	{
		let buf = &mut port.backend;
		for response in &responses {
			buf.push(*response);
		}
	}
	port.set_packet_handler(|data, dir| {
		if let Ok(mut buffer) = captured.try_borrow_mut() {
			buffer.push((data.to_vec(), dir));
		}
	});

	port.command((1, 3, "get pos")).unwrap();
	let _ = port
		.response_n::<AnyResponse, _>(3, check::strict())
		.unwrap();

	let mut expected = Vec::with_capacity(4);
	expected.push((b"/1 3 get pos\n".to_vec(), super::Direction::Tx));
	for response in responses {
		expected.push((response.to_vec(), super::Direction::Recv));
	}
	assert_eq!(captured.borrow().as_slice(), expected.as_slice());
}

mod response_check {
	use super::*;

	/// Check that the default response check after creating a port is
	/// `check::strict()`, which behaves as follows:
	///   * replies with any warning or an RJ flag should raise an error
	///   * alerts with any warning should raise an error
	///   * infos are not checked
	#[test]
	fn default() {
		let mut port = Port::open_mock();
		check_cases! { port,
			ok case b"@01 1 OK IDLE -- 0\r\n" via |p| p.command_reply(""),
			ok case b"@01 1 OK IDLE -- 0\r\n" via |p| p.response::<Reply>(),
			ok case b"@01 1 OK IDLE -- 0\r\n" via |p| p.response::<AnyResponse>(),

			ok case b"!01 1 IDLE -- 0\r\n" via |p| p.response::<Alert>(),
			ok case b"!01 1 IDLE -- 0\r\n" via |p| p.response::<AnyResponse>(),

			ok case b"#01 1 some info\r\n" via |p| p.response::<Info>(),
			ok case b"#01 1 some info\r\n" via |p| p.response::<AnyResponse>(),

			ok case  b"@01 1 OK IDLE -- 0\r\n#01 1 foo\r\n#01 1 bar\r\n@01 1 01 OK IDLE -- 0\r\n" via |p| p.command_reply_infos("", check::strict()),
			err case b"@01 1 OK IDLE WR 0\r\n#01 1 foo\r\n#01 1 bar\r\n@01 1 01 OK IDLE -- 0\r\n" via |p| p.command_reply_infos("", check::strict()) => AsciiCheckWarningError,
		}
	}

	/// Ensure that check overrides are respected
	#[test]
	fn overrides() {
		let mut port = Port::open_mock();

		check_cases! { port,
			ok case b"@01 1 OK IDLE WR 0\r\n" via |p| p.command_reply("").unwrap().flag_ok(),

			ok case b"@01 1 RJ IDLE -- 0\r\n" via |p| p.command_reply("").unwrap().check(check::warning_is_none()),

			ok case b"@01 1 RJ IDLE -- 0 \r\n" via |p| p.poll_until("", check::predicate(|_| true), |_| true),
			ok case b"@01 1 RJ IDLE -- 0 \r\n" via |p| p.poll_until_idle(1, check::predicate(|_| true)),

			ok case b"@01 1 OK IDLE WR 0\r\n#01 1 foo\r\n#01 1 bar\r\n@01 1 01 OK IDLE -- 0\r\n" via |p| p.command_reply_infos("", check::unchecked()),
		}
	}
}

// Poison a port
fn poison_port<Tag>(port: &mut Port<'_, Mock, Tag>) {
	use std::{io, time::Duration};
	let mut guard = port.timeout_guard(Some(Duration::from_secs(1))).unwrap();
	guard
		.backend
		.set_read_timeout_error(Some(io::Error::new(io::ErrorKind::Other, "OOPS!")));
}

/// Assert that a result contains a poisoning error.
fn assert_poisoned<T: std::fmt::Debug>(result: Result<T, AsciiError>) {
	assert!(result.is_err());
	let err = result.unwrap_err();
	assert!(is_poisoning_error(&err), "{err} is not a poisoning error");
}

/// Assert that the result does not contain a poisoning error.
fn assert_not_poisoned<T: std::fmt::Debug>(result: Result<T, AsciiError>) {
	if let Err(ref err) = result {
		assert!(!is_poisoning_error(err), "{err} is a poisoning error");
	}
}

/// Return true if the error is a poisoning error.
fn is_poisoning_error(err: &AsciiError) -> bool {
	use std::io;

	let mut poisoning = false;
	if let AsciiError::Io(e) = err {
		if e.kind() == io::ErrorKind::Other {
			let message = format!("{e}");
			poisoning = message.starts_with("failed to reset") && message.contains("OOPS!");
		}
	}
	poisoning
}

/// Generate a test with the given Port `$method`, which ensures that calling
/// `$method` with the specified `$args` surfaces the unhandled error created
/// in the [`TimeoutGuard`]'s drop implementation.
macro_rules! make_poison_test {
    // Case: the Port method to call includes types parameters
    (
        $method:ident::< $($types:ty),+ > $(,)? $($args:expr),*
    ) => {
        make_poison_test!(@create $method, $($types),+ ; $($args),* );
    };

    // Case: the Port method to call does not include type parameters
    (
        $method:ident $(,)? $($args:expr),*
    ) => {
        make_poison_test!(@create $method, ; $($args),* );
    };

    // Internal case: create the test function.
    (@create $method:ident, $($types:ty),* ; $($args:expr),* ) => {
        paste::paste! { // For generating new identifiers
            #[test]
            fn [<poisoned_ $method>]() {
                // Create a poisoned port and check that $method surfaces the
                // poisoning error.
                let mut port = Port::open_mock();
                poison_port(&mut port);
                let result = port.$method::< $($types),* >( $($args),* );
                assert_poisoned(result.map_err(Into::into));

                // Subsequent calls should not surface the poisoning error.
                let result = port.$method::< $($types),* >( $($args),* );
                assert_not_poisoned(result.map_err(Into::into));
            }
        }
    };
}

make_poison_test!(command, "");
make_poison_test!(command_reply, "");
make_poison_test!(command_reply_infos, "", unchecked());
make_poison_test!(command_reply_n, "", 1, unchecked());
make_poison_test!(poll_until, "", check::flag_ok(), |_| true);
make_poison_test!(poll_until_idle, 1, check::flag_ok());
make_poison_test!(response::<AnyResponse>);
make_poison_test!(response_n::<AnyResponse, _>, 1, unchecked::<AnyResponse>());
make_poison_test!(responses_until_timeout, unchecked::<AnyResponse>());
make_poison_test!(timeout_guard, None);

/// Calling `open_dyn()` should return a type that implements `Send`.
#[test]
fn port_send_bounds() {
	if let Ok(port) = Port::open_serial_options().open_dyn("...") {
		let mut port = port.try_into_send().unwrap();

		std::thread::spawn(move || {
			// Do something to use the port and cause the move.
			let _ = port.set_read_timeout(None);
		});
	}

	if let Ok(port) = Port::open_tcp_options().open_dyn("...") {
		let mut port = port.try_into_send().unwrap();

		std::thread::spawn(move || {
			// Do something to use the port and cause the move.
			let _ = port.set_read_timeout(None);
		});
	}
}
