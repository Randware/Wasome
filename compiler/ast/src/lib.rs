mod expression;
mod statement;
mod block;
mod top_level;
mod symbol;
mod data_type;

pub fn add(left: u64, right: u64) -> u64 {
    left + right
}


/** This compares two values
This is useful for returning with the ? operator if values are not equal
@params
left, right: The values to compare
@return
None if not equal
Some if equal
*/
fn eq_return_option<T: PartialEq>(left: T, right: T) -> Option<()>
{
    if left == right
    {
        return Some(())
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
