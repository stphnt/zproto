//! Parsing tests

use super::{
    get_packet_contents,
    packet::{InnerPacket, RefPacket},
    token::{RefTokens, Token, Tokens},
    visitor::{Client, PacketKind, Visitor},
};
use crate::ascii::{Flag, Status, Warning};

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
struct Combined<'a>(RefTokens<'a>, RefPacket<'a>);

impl<'a> Combined<'a> {
    fn new() -> Self {
        Combined(RefTokens::new_default(), RefPacket::new_default())
    }
}

impl<'a> Visitor<'a> for Combined<'a> {
    type Output = (RefTokens<'a>, RefPacket<'a>);

    fn separator(&mut self, bytes: &'a [u8]) {
        Visitor::separator(&mut self.1, bytes);
        Visitor::separator(&mut self.0, bytes);
    }
    fn kind(&mut self, value: PacketKind, bytes: &'a [u8]) {
        Visitor::kind(&mut self.1, value, bytes);
        Visitor::kind(&mut self.0, value, bytes);
    }
    fn device_address(&mut self, value: u8, bytes: &'a [u8]) {
        Visitor::device_address(&mut self.1, value, bytes);
        Visitor::device_address(&mut self.0, value, bytes);
    }
    fn axis_number(&mut self, value: u8, bytes: &'a [u8]) {
        Visitor::axis_number(&mut self.1, value, bytes);
        Visitor::axis_number(&mut self.0, value, bytes);
    }
    fn message_id(&mut self, value: u8, bytes: &'a [u8]) {
        Visitor::message_id(&mut self.1, value, bytes);
        Visitor::message_id(&mut self.0, value, bytes);
    }
    fn flag(&mut self, value: Flag, bytes: &'a [u8]) {
        Visitor::flag(&mut self.1, value, bytes);
        Visitor::flag(&mut self.0, value, bytes);
    }
    fn status(&mut self, value: Status, bytes: &'a [u8]) {
        Visitor::status(&mut self.1, value, bytes);
        Visitor::status(&mut self.0, value, bytes);
    }
    fn warning(&mut self, value: Warning, bytes: &'a [u8]) {
        Visitor::warning(&mut self.1, value, bytes);
        Visitor::warning(&mut self.0, value, bytes);
    }
    fn cont(&mut self, bytes: &'a [u8]) {
        Visitor::cont(&mut self.1, bytes);
        Visitor::cont(&mut self.0, bytes);
    }
    fn cont_count(&mut self, value: u8, bytes: &'a [u8]) {
        Visitor::cont_count(&mut self.1, value, bytes);
        Visitor::cont_count(&mut self.0, value, bytes);
    }
    fn data_word(&mut self, bytes: &'a [u8]) {
        Visitor::data_word(&mut self.1, bytes);
        Visitor::data_word(&mut self.0, bytes);
    }
    fn data(&mut self, bytes: &'a [u8]) {
        Visitor::data(&mut self.1, bytes);
        Visitor::data(&mut self.0, bytes);
    }
    fn content(&mut self, bytes: &'a [u8]) {
        Visitor::content(&mut self.1, bytes);
        Visitor::content(&mut self.0, bytes);
    }
    fn more_packets_marker(&mut self, bytes: &'a [u8]) {
        Visitor::more_packets_marker(&mut self.1, bytes);
        Visitor::more_packets_marker(&mut self.0, bytes);
    }
    fn checksum_marker(&mut self, bytes: &'a [u8]) {
        Visitor::checksum_marker(&mut self.1, bytes);
        Visitor::checksum_marker(&mut self.0, bytes);
    }
    fn checksum(&mut self, value: u32, bytes: &'a [u8]) {
        Visitor::checksum(&mut self.1, value, bytes);
        Visitor::checksum(&mut self.0, value, bytes);
    }
    fn termination(&mut self, bytes: &'a [u8]) {
        Visitor::termination(&mut self.1, bytes);
        Visitor::termination(&mut self.0, bytes);
    }

    fn start_visit(&mut self, bytes: &'a [u8]) {
        Visitor::start_visit(&mut self.1, bytes);
        Visitor::start_visit(&mut self.0, bytes);
    }
    fn finish_visit(self) -> Self::Output {
        (self.0, self.1)
    }
}

type Item<'a> = (RefTokens<'a>, RefPacket<'a>);

fn parse_combined<'a>(packet: &'a [u8]) -> Option<Item<'a>> {
    let client = Client::new(Combined::new(), packet);
    client.parse().ok()
}

#[test]
fn parse() {
    struct Case {
        input: &'static [u8],
        expected: Option<Item<'static>>,
    }

    let cases = &[
        Case {
            input: b"/".as_slice(),
            expected: None,
        },
        Case {
            input: b"@".as_slice(),
            expected: None,
        },
        Case {
            input: b"!".as_slice(),
            expected: None,
        },
        Case {
            input: b"#".as_slice(),
            expected: None,
        },
        Case {
            input: b"\t /\n".as_slice(), // Leading whitespace
            expected: None,
        },
        Case {
            input: b"/\n ".as_slice(), // Trailing whitespace
            expected: None,
        },
        Case {
            input: b"/\xFF\n".as_slice(), // Non-ASCII characters
            expected: None,
        },
        // Commands
        Case {
            input: b"/\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/\n".as_slice(),
                    items: vec![(Token::Kind, 1), (Token::Terminator, 1)],
                },
                InnerPacket {
                    packet: b"/\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (0, 0),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"".as_slice(),
                    content: b"".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/\r".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/\r".as_slice(),
                    items: vec![(Token::Kind, 1), (Token::Terminator, 1)],
                },
                InnerPacket {
                    packet: b"/\r".as_slice(),
                    kind: PacketKind::Command,
                    target: (0, 0),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"".as_slice(),
                    content: b"".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/\r\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/\r\n".as_slice(),
                    items: vec![(Token::Kind, 1), (Token::Terminator, 2)],
                },
                InnerPacket {
                    packet: b"/\r\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (0, 0),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"".as_slice(),
                    content: b"".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/hi\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/hi\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DataWord, 2),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/hi\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (0, 0),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"hi".as_slice(),
                    content: b"hi".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/1\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/1\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 1),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (1, 0),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"".as_slice(),
                    content: b"1".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/1 2\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/1 2\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 1),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1 2\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"".as_slice(),
                    content: b"1 2".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/1 2 3\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/1 2 3\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 1),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::MessageId, 1),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1 2 3\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (1, 2),
                    id: Some(3),
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"".as_slice(),
                    content: b"1 2 3".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/1 2 3 hi\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/1 2 3 hi\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 1),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::MessageId, 1),
                        (Token::Separator, 1),
                        (Token::DataWord, 2),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1 2 3 hi\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (1, 2),
                    id: Some(3),
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"hi".as_slice(),
                    content: b"1 2 3 hi".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/1 2 hi\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/1 2 hi\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 1),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::DataWord, 2),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1 2 hi\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"hi".as_slice(),
                    content: b"1 2 hi".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/1 hi\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/1 hi\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 1),
                        (Token::Separator, 1),
                        (Token::DataWord, 2),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1 hi\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (1, 0),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"hi".as_slice(),
                    content: b"1 hi".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/1hi\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/1hi\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DataWord, 3),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1hi\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (0, 0),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"1hi".as_slice(),
                    content: b"1hi".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/1 2hi\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/1 2hi\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 1),
                        (Token::Separator, 1),
                        (Token::DataWord, 3),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1 2hi\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (1, 0),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"2hi".as_slice(),
                    content: b"1 2hi".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/1 2 3hi\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/1 2 3hi\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 1),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::DataWord, 3),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1 2 3hi\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"3hi".as_slice(),
                    content: b"1 2 3hi".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/1  2\t3hi\n".as_slice(), // Extra and differing separating whitespace
            expected: Some((
                Tokens {
                    packet: b"/1  2\t3hi\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 1),
                        (Token::Separator, 2),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::DataWord, 3),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1  2\t3hi\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"3hi".as_slice(),
                    content: b"1  2\t3hi".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/OK IDLE --\n".as_slice(), // Command that looks like a reply
            expected: Some((
                Tokens {
                    packet: b"/OK IDLE --\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DataWord, 2),
                        (Token::Separator, 1),
                        (Token::DataWord, 4),
                        (Token::Separator, 1),
                        (Token::DataWord, 2),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/OK IDLE --\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (0, 0),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"OK IDLE --".as_slice(),
                    content: b"OK IDLE --".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"/1 2 tools echo command\\:12\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/1 2 tools echo command\\:12\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 1),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::DataWord, 5),
                        (Token::Separator, 1),
                        (Token::DataWord, 4),
                        (Token::Separator, 1),
                        (Token::DataWord, 7),
                        (Token::MorePacketsMarker, 1),
                        (Token::ChecksumMarker, 1),
                        (Token::Checksum, 2),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1 2 tools echo command\\:12\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"tools echo command".as_slice(),
                    content: b"1 2 tools echo command\\".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: true,
                    checksum: Some(18),
                }
                .into(),
            )),
        },
        Case {
            input: b"/1 2 cont 1 everything else:10\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"/1 2 cont 1 everything else:10\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 1),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::Cont, 4),
                        (Token::Separator, 1),
                        (Token::ContCount, 1),
                        (Token::Separator, 1),
                        (Token::DataWord, 10),
                        (Token::Separator, 1),
                        (Token::DataWord, 4),
                        (Token::ChecksumMarker, 1),
                        (Token::Checksum, 2),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"/1 2 cont 1 everything else:10\n".as_slice(),
                    kind: PacketKind::Command,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"everything else".as_slice(),
                    content: b"1 2 cont 1 everything else".as_slice(),
                    cont: true,
                    cont_count: Some(1),
                    more_packets: false,
                    checksum: Some(16),
                }
                .into(),
            )),
        },
        // Replies
        Case {
            input: b"@OK IDLE --\n".as_slice(),
            expected: None,
        },
        Case {
            input: b"@01 2 OK IDLE -- 1.2 3.4\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"@01 2 OK IDLE -- 1.2 3.4\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 2),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::Flag, 2),
                        (Token::Separator, 1),
                        (Token::Status, 4),
                        (Token::Separator, 1),
                        (Token::Warning, 2),
                        (Token::Separator, 1),
                        (Token::DataWord, 3),
                        (Token::Separator, 1),
                        (Token::DataWord, 3),
                        (Token::Terminator, 1),
                    ],
                },
                InnerPacket {
                    packet: b"@01 2 OK IDLE -- 1.2 3.4\n".as_slice(),
                    kind: PacketKind::Reply,
                    target: (1, 2),
                    id: None,
                    flag: Some(Flag::Ok),
                    status: Some(Status::Idle),
                    warning: Some(Warning::NONE),
                    data: b"1.2 3.4".as_slice(),
                    content: b"01 2 OK IDLE -- 1.2 3.4".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        // Infos
        Case {
            input: b"#\r\n".as_slice(),
            expected: None,
        },
        Case {
            input: b"#01 2\r\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"#01 2\r\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 2),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Terminator, 2),
                    ],
                },
                InnerPacket {
                    packet: b"#01 2\r\n".as_slice(),
                    kind: PacketKind::Info,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"".as_slice(),
                    content: b"01 2".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"#01 2 info message \r\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"#01 2 info message \r\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 2),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::DataWord, 4),
                        (Token::Separator, 1),
                        (Token::DataWord, 7),
                        (Token::Separator, 1),
                        (Token::Terminator, 2),
                    ],
                },
                InnerPacket {
                    packet: b"#01 2 info message \r\n".as_slice(),
                    kind: PacketKind::Info,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"info message".as_slice(),
                    content: b"01 2 info message ".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"#01 2 info message :12\r\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"#01 2 info message :12\r\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 2),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::DataWord, 4),
                        (Token::Separator, 1),
                        (Token::DataWord, 7),
                        (Token::Separator, 1),
                        (Token::ChecksumMarker, 1),
                        (Token::Checksum, 2),
                        (Token::Terminator, 2),
                    ],
                },
                InnerPacket {
                    packet: b"#01 2 info message :12\r\n".as_slice(),
                    kind: PacketKind::Info,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"info message".as_slice(),
                    content: b"01 2 info message ".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: Some(18),
                }
                .into(),
            )),
        },
        Case {
            input: b"#01 2 info message \\:12\r\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"#01 2 info message \\:12\r\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 2),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::DataWord, 4),
                        (Token::Separator, 1),
                        (Token::DataWord, 7),
                        (Token::Separator, 1),
                        (Token::MorePacketsMarker, 1),
                        (Token::ChecksumMarker, 1),
                        (Token::Checksum, 2),
                        (Token::Terminator, 2),
                    ],
                },
                InnerPacket {
                    packet: b"#01 2 info message \\:12\r\n".as_slice(),
                    kind: PacketKind::Info,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"info message".as_slice(),
                    content: b"01 2 info message \\".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: true,
                    checksum: Some(18),
                }
                .into(),
            )),
        },
        Case {
            input: b"#01 2 cont info message \\:12\r\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"#01 2 cont info message \\:12\r\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 2),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::Cont, 4),
                        (Token::Separator, 1),
                        (Token::DataWord, 4),
                        (Token::Separator, 1),
                        (Token::DataWord, 7),
                        (Token::Separator, 1),
                        (Token::MorePacketsMarker, 1),
                        (Token::ChecksumMarker, 1),
                        (Token::Checksum, 2),
                        (Token::Terminator, 2),
                    ],
                },
                InnerPacket {
                    packet: b"#01 2 cont info message \\:12\r\n".as_slice(),
                    kind: PacketKind::Info,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"info message".as_slice(),
                    content: b"01 2 cont info message \\".as_slice(),
                    cont: true,
                    cont_count: None,
                    more_packets: true,
                    checksum: Some(18),
                }
                .into(),
            )),
        },
        Case {
            input: b"#01 2 cont 56 message \\:12\r\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"#01 2 cont 56 message \\:12\r\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 2),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::Cont, 4),
                        (Token::Separator, 1),
                        (Token::DataWord, 2),
                        (Token::Separator, 1),
                        (Token::DataWord, 7),
                        (Token::Separator, 1),
                        (Token::MorePacketsMarker, 1),
                        (Token::ChecksumMarker, 1),
                        (Token::Checksum, 2),
                        (Token::Terminator, 2),
                    ],
                },
                InnerPacket {
                    packet: b"#01 2 cont 56 message \\:12\r\n".as_slice(),
                    kind: PacketKind::Info,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: None,
                    warning: None,
                    data: b"56 message".as_slice(),
                    content: b"01 2 cont 56 message \\".as_slice(),
                    cont: true,
                    cont_count: None,
                    more_packets: true,
                    checksum: Some(18),
                }
                .into(),
            )),
        },
        // Alerts
        Case {
            input: b"!\r\n".as_slice(),
            expected: None,
        },
        Case {
            input: b"!01 2 IDLE -- \r\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"!01 2 IDLE -- \r\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 2),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::Status, 4),
                        (Token::Separator, 1),
                        (Token::Warning, 2),
                        (Token::Separator, 1),
                        (Token::Terminator, 2),
                    ],
                },
                InnerPacket {
                    packet: b"!01 2 IDLE -- \r\n".as_slice(),
                    kind: PacketKind::Alert,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: Some(Status::Idle),
                    warning: Some(Warning::NONE),
                    data: b"".as_slice(),
                    content: b"01 2 IDLE -- ".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"!01 2 BUSY FF \r\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"!01 2 BUSY FF \r\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 2),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::Status, 4),
                        (Token::Separator, 1),
                        (Token::Warning, 2),
                        (Token::Separator, 1),
                        (Token::Terminator, 2),
                    ],
                },
                InnerPacket {
                    packet: b"!01 2 BUSY FF \r\n".as_slice(),
                    kind: PacketKind::Alert,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: Some(Status::Busy),
                    warning: Some(Warning::from(b"FF")),
                    data: b"".as_slice(),
                    content: b"01 2 BUSY FF ".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: false,
                    checksum: None,
                }
                .into(),
            )),
        },
        Case {
            input: b"!01 2 BUSY FF words go here\\:5F\r\n".as_slice(),
            expected: Some((
                Tokens {
                    packet: b"!01 2 BUSY FF words go here\\:5F\r\n".as_slice(),
                    items: vec![
                        (Token::Kind, 1),
                        (Token::DeviceAddress, 2),
                        (Token::Separator, 1),
                        (Token::AxisNumber, 1),
                        (Token::Separator, 1),
                        (Token::Status, 4),
                        (Token::Separator, 1),
                        (Token::Warning, 2),
                        (Token::Separator, 1),
                        (Token::DataWord, 5),
                        (Token::Separator, 1),
                        (Token::DataWord, 2),
                        (Token::Separator, 1),
                        (Token::DataWord, 4),
                        (Token::MorePacketsMarker, 1),
                        (Token::ChecksumMarker, 1),
                        (Token::Checksum, 2),
                        (Token::Terminator, 2),
                    ],
                },
                InnerPacket {
                    packet: b"!01 2 BUSY FF words go here\\:5F\r\n".as_slice(),
                    kind: PacketKind::Alert,
                    target: (1, 2),
                    id: None,
                    flag: None,
                    status: Some(Status::Busy),
                    warning: Some(Warning::from(b"FF")),
                    data: b"words go here".as_slice(),
                    content: b"01 2 BUSY FF words go here\\".as_slice(),
                    cont: false,
                    cont_count: None,
                    more_packets: true,
                    checksum: Some(95),
                }
                .into(),
            )),
        },
        Case {
            input: b"!01 2 IDLE --- \r\n".as_slice(),
            expected: None,
        },
    ];

    for (i, case) in cases.iter().enumerate() {
        let input_str = String::from_utf8_lossy(case.input);
        let input_str = input_str.as_ref().trim_end();
        println!("Case {i}: {input_str}");
        assert_eq!(parse_combined(case.input), case.expected);
    }
}

#[test]
fn test_get_packet_contents() {
    let expected = b"1 2 OK IDLE --";
    let cases = &[
        b"  /1 2 OK IDLE --\r    ",
        b"  /1 2 OK IDLE --\n    ",
        b"  /1 2 OK IDLE --\r\n   ",
        b"  /1 2 OK IDLE --:12\r\n",
        b"  @1 2 OK IDLE --\r\n   ",
        b"  !1 2 OK IDLE --\r\n   ",
        b"  #1 2 OK IDLE --\r\n   ",
    ];

    for (i, case) in cases.iter().enumerate() {
        assert_eq!(get_packet_contents(*case), expected, "Case {} failed", i);
    }
}
