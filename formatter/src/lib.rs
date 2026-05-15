mod classify;
mod config;
mod formatter;

use formatter::Formatter;

// Entry point for formatting a source string.
pub fn format(source: String) -> String {
    let tokens: Vec<_> = lexer::lex(&source).filter_map(|r| r.ok()).collect();

    Formatter::new().format(&tokens)
}

#[cfg(test)]
mod tests;
