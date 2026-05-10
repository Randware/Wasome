mod lld;

#[derive(Debug)]
pub struct LLD {}

pub struct OFile(pub Vec<u8>);

impl From<Vec<u8>> for OFile {
    fn from(value: Vec<u8>) -> Self {
        OFile(value)
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn it_works() {
//         assert_eq!(4, 4);
//     }
// }
