mod classify;
mod config;
mod formatter;

use formatter::Formatter;

// Entry point for formatting a source string.
pub fn format(source: String) -> Result<String, lexer::tokens::LexError> {
    let tokens = lexer::lex(&source).collect::<Result<Vec<_>, _>>()?;

    Ok(Formatter::new().format(&tokens))
}

#[cfg(test)]
mod tests;
