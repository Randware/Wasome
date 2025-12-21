mod error;
mod snippet;

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf};

    use source::{SourceFile, types::Span};

    use crate::{
        error::{Error, ErrorKind},
        snippet::Snippet,
    };

    #[test]
    fn test_print() {
        let file = fs::read_to_string("./test.waso").unwrap();

        println!("{}", file);

        let err = Error::builder()
            .kind(ErrorKind::Error)
            .message("This is an error")
            .snippet(
                Snippet::builder()
                    .source(1)
                    .annotate_many([(1..2), (3..4)], "This is not valid")
                    .build(),
            )
            .snippet(
                Snippet::builder()
                    .source(SourceFile::new(
                        PathBuf::from("./test.waso"),
                        "This is a source file for testing".to_string(),
                    ))
                    .annotate(1..2, "You cannot do this")
                    .build(),
            )
            .build();

        err.print().unwrap();
    }

    #[test]
    fn test_annotate_snippets() {}
}
