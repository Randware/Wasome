mod diagnostic;
mod renderer;
mod source;

#[cfg(test)]
mod tests {
    use crate::diagnostic::{Diagnostic, Level, Snippet};

    #[test]
    fn test() {
        let error = Diagnostic::builder()
            .level(Level::Error)
            .message("Expected expression")
            .code("E0123")
            // .snippet(
            //     Snippet::builder()
            //         .file(1)
            //         .annotate(24..25, "Expression expected here")
            //         .annotate_many([0..2, 24..25], "Related context")
            //         .build(),
            // )
            // .snippet(
            //     Snippet::builder()
            //         .file(2)
            //         .annotate(24..25, "Expression expected here")
            //         .annotate_many([0..2, 24..25], "Related context")
            //         .build(),
            // )
            .help("Try adding a number, e.g. '1 + 2'")
            .build();

        error.print().unwrap()
    }
}
