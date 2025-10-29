use bon::Builder;
use shared::code_reference::{CodeArea, CodeLocation};

#[derive(Builder)]
pub struct Snippet {
    #[builder(field)]
    pub annotations: Vec<(
        Box<dyn Iterator<Item = (CodeLocation, CodeLocation)>>,
        &'static str,
    )>,

    pub source: CodeArea,
}

impl<S: snippet_builder::State> SnippetBuilder<S> {
    pub fn annotate(
        mut self,
        locations: impl IntoIterator<Item = (CodeLocation, CodeLocation)> + 'static,
        message: &'static str,
    ) -> Self {
        self.annotations
            .push((Box::new(locations.into_iter()), message));
        self
    }
}
