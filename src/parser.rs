use crate::lex;

#[derive(Debug, Clone, Copy)]
pub struct TakeError {
    pub begin: usize,
}

#[derive(Clone)]
pub struct Parser<'a> {
    pub src: &'a str,
    pub cursor: usize,
    pub name_substitutions: &'a [lex::NameSubstitute<'a>],
    pub name_substitution_stack: Vec<&'a lex::NameSubstitute<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str, name_substitutions: &'a [lex::NameSubstitute<'a>]) -> Self {
        Self {
            src,
            cursor: 0,
            name_substitutions,
            name_substitution_stack: vec![],
        }
    }

    pub fn take(&mut self) -> Result<char, TakeError> {
        if let Some(char) = self.src[self.cursor..].chars().next() {
            self.cursor += char.len_utf8();
            Ok(char)
        } else {
            Err(TakeError { begin: self.cursor })
        }
    }

    pub fn ended(&self) -> bool {
        self.src.len() <= self.cursor
    }

    pub fn peek(&self) -> Option<char> {
        self.src[self.cursor..].chars().next()
    }

    pub fn try_parse<T, E>(&mut self, func: impl Fn(&mut Parser) -> Result<T, E>) -> Result<T, E> {
        let previous_cursor_position = self.cursor;
        let result = func(self);
        if result.is_err() {
            self.cursor = previous_cursor_position;
        }
        result
    }
}
