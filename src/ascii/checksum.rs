//! Types for producing and verifying ASCII packet checksums
use crate::ascii::parse::get_packet_contents;

/// A Longitudinal Redundancy Check hasher.
#[derive(Debug, Default)]
pub(crate) struct Lrc {
    /// The hash value
    sum: u32,
}

impl Lrc {
    /// Update the Lrc with the specified byte.
    pub fn update(&mut self, byte: u8) {
        self.sum = (self.sum + byte as u32) & 0xFF;
    }

    /// Finish calculating the Lrc hash.
    ///
    /// The hasher's state is reset.
    pub fn finish(&mut self) -> u32 {
        let sum = ((self.sum ^ 0xFF) + 1) & 0xFF;
        self.sum = 0;
        sum
    }

    /// Hash the input
    pub fn hash(input: &[u8]) -> u32 {
        let mut hasher = Lrc::default();
        for byte in input {
            hasher.update(*byte);
        }
        hasher.finish()
    }

    /// Verify if the hash matches the contents of the ASCII packet.
    ///
    /// The `input` is expected to be a valid ASCII packet. Only the
    /// appropriate contents of the message are hashed.
    pub fn verify_packet(input: &[u8], hash: u32) -> bool {
        let input = get_packet_contents(input);
        let sum: u32 = input.iter().fold(0u32, |sum, b| *b as u32 + sum);
        0 == ((sum + hash) & 0xFF)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lrc() {
        assert!(Lrc::verify_packet(b"/01 tools echo:8F\r\n", 143));
        assert!(!Lrc::verify_packet(b"/01 tools echo:8F\r\n", 142));
    }
}
