use super::*;

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
