use std::sync::RwLock;

/// The next id. It is only ever incremented
static NEXT_ID: RwLock<u64> = RwLock::new(0);

/** Returns the next id.
Two values returned by different calls to this will never be the same
*/
fn next_id() -> u64 {
    let mut id_lock = NEXT_ID
        .write()
        // write returns an error if and only if the lock is poisoned
        // If the lock is poisoned, we panicked
        // Therefore, there has already been an unrecoverable error
        .expect("The id lock is poisoned");
    // Get a copy of the id
    let next_id = *id_lock;
    // Increment the shared id so the next one gets a different one
    // Overflows should not be a problem as an u64 can hold 2^64 different values
    *id_lock += 1;
    next_id
}

/** An id
# Obtaining
The primary way to obtain ids is by calling the new() method.
Alternatively, an existing id can be cloned
# Equality
Equality with ids follows two simple rules: <br>
Ids obtained from two different calls to new() will never be equal <br>
Cloned ids will always be equal

As required by the Eq trait, equality is transitive
# Example
```
use ast::id::Id;
// Create two ids
let id1 = Id::new();
let id2 = Id::new();

// They are not equal
assert_ne!(id1, id2);

// Clones originating from the same id are equal
let id1_clone1 = id1.clone();
assert_eq!(id1, id1_clone1);
let id1_clone2 = id1.clone();
assert_eq!(id1, id1_clone2);
assert_eq!(id1_clone1, id1_clone2);

let id2_clone = id2.clone();

// Clones originating from different ids are not equal
assert_ne!(id1_clone1, id2_clone);
```
*/
#[derive(Debug, PartialEq, Eq, Clone, Hash, Default)]
pub struct Id {
    inner: u64,
}

impl Id {
    pub fn new() -> Self {
        Self { inner: next_id() }
    }
}

#[cfg(test)]
mod tests {
    use crate::id::Id;

    #[test]
    fn multiple_ids_should_not_be_equal() {
        let ids = (0..100).map(|_| Id::new()).collect::<Vec<Id>>();
        ids.iter().enumerate().for_each(|(index_1, id_1)| {
            ids.iter()
                .enumerate()
                .filter(|(index_2, _)| index_1 != *index_2)
                .for_each(|(_, id_2)| assert_ne!(id_1, id_2))
        })
    }

    #[test]
    fn cloned_ids_should_be_equal() {
        let id = Id::new();
        assert_eq!(id, id.clone());
        assert_eq!(id.clone(), id.clone());
        assert_eq!(id.clone().clone(), id.clone());
    }
}
