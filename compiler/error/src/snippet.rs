use bon::Builder;
use shared::code_reference::{CodeArea, CodeLocation};

pub(crate) struct Annotation {
    locations: Box<dyn Iterator<Item = (CodeLocation, CodeLocation)>>,
    message: String,
}

#[derive(Builder)]
pub struct Snippet {
    #[builder(field)]
    pub(crate) annotations: Vec<Annotation>,

    pub(crate) source: CodeArea,
}

impl<S: snippet_builder::State> SnippetBuilder<S> {
    pub fn annotate_many(
        mut self,
        locations: impl IntoIterator<Item = (CodeLocation, CodeLocation)> + 'static,
        message: &str,
    ) -> Self {
        self.annotations.push(Annotation {
            locations: Box::new(locations.into_iter()),
            message: message.to_string(),
        });
        self
    }

    pub fn annotate(mut self, location: (CodeLocation, CodeLocation), message: &str) -> Self {
        self.annotations.push(Annotation {
            locations: Box::new(std::iter::once(location)),
            message: message.to_string(),
        });
        self
    }
}
