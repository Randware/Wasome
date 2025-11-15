mod error;
mod snippet;

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf};

    use shared::{
        code_file::CodeFile,
        code_reference::{CodeArea, CodeLocation},
    };

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
                    .source(
                        CodeArea::new(
                            CodeLocation::new(1, 2),
                            CodeLocation::new(2, 3),
                            CodeFile::new(PathBuf::from("test.waso")),
                        )
                        .unwrap(),
                    )
                    .annotate_many(
                        [
                            (CodeLocation::new(1, 2), CodeLocation::new(1, 10)),
                            (CodeLocation::new(3, 5), CodeLocation::new(3, 15)),
                        ],
                        "This is not valid",
                    )
                    .build(),
            )
            .snippet(
                Snippet::builder()
                    .source(
                        CodeArea::new(
                            CodeLocation::new(1, 2),
                            CodeLocation::new(2, 3),
                            CodeFile::new(PathBuf::from("test.waso")),
                        )
                        .unwrap(),
                    )
                    .annotate(
                        (CodeLocation::new(1, 2), CodeLocation::new(1, 10)),
                        "You cannot do this",
                    )
                    .build(),
            )
            .build();

        err.print().unwrap();
    }

    #[test]
    fn test_annotate_snippets() {}
}
