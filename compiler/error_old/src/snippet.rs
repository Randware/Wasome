use std::{ops::Range};

use bon::Builder;
use source::types::{FileID}

pub(crate) struct Annotation {
    locations: Box<dyn Iterator<Item = Range<u32>>>,
    message: String,
}

#[derive(Builder)]
pub struct Snippet {
    #[builder(field)]
    pub(crate) annotations: Vec<Annotation>,

    pub(crate) source: FileID,
}

impl<S: snippet_builder::State> SnippetBuilder<S> {
    pub fn annotate_many(
        mut self,
        locations: impl IntoIterator<Item = Range<u32>> + 'static,
        message: &str,
    ) -> Self {

        self.annotations.push(Annotation {
            locations: Box::new(locations.into_iter()),
            message: message.to_string(),
        });
        self
    }

    pub fn annotate(mut self, location: Range<u32>, message: &str) -> Self {
        self.annotations.push(Annotation {
            locations: Box::new(std::iter::once(location)),
            message: message.to_string(),
        });
        self
    }
}
