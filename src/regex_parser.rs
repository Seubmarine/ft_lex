use crate::parser::{Parser, TakeError};

#[derive(Debug, Clone, Copy)]
enum BracketInside {
    Single(char),
    Range(char, char),
    CharacterClass(CharacterClass),
}

#[derive(Debug, Clone)]
struct Bracket {
    is_negative: bool,
    insides: Vec<BracketInside>,
}

pub fn char_cmp(c: char) -> impl Fn(&mut Parser) -> Result<char, TakeError> {
    move |parser: &mut Parser<'_>| {
        if parser.take()? == c {
            Ok(c)
        } else {
            Err(TakeError {
                begin: parser.cursor - 1,
            })
        }
    }
}

pub fn str_cmp<'a>(s: &'a str) -> impl Fn(&mut Parser) -> Result<&'a str, TakeError> {
    move |parser: &mut Parser<'_>| {
        for c in s.chars() {
            if parser.take()? != c {
                return Err(TakeError {
                    begin: parser.cursor,
                });
            }
        }
        return Ok(s);
    }
}

trait ParsePattern: Sized {
    type ResultOk;
    type ResultErr;
    fn into_fn(self) -> impl Fn(&mut Parser) -> Result<Self::ResultOk, Self::ResultErr>;
}

impl ParsePattern for char {
    type ResultOk = char;

    type ResultErr = TakeError;

    fn into_fn(self) -> impl Fn(&mut Parser) -> Result<Self::ResultOk, Self::ResultErr> {
        char_cmp(self)
    }
}

/*
[:alnum:]   [:cntrl:]   [:lower:]   [:space:]
[:alpha:]   [:digit:]   [:print:]   [:upper:]
[:blank:]   [:graph:]   [:punct:]   [:xdigit:]
*/

#[derive(Debug, Clone, Copy)]
enum CharacterClass {
    Alnum,
    Cntrl,
    Lower,
    Space,
    Alpha,
    Digit,
    Print,
    Upper,
    Blank,
    Graph,
    Punct,
    Xdigit,
}

impl TryFrom<&str> for CharacterClass {
    type Error = TakeError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "alnum" => Self::Alnum,
            "cnrtl" => Self::Cntrl,
            "lower" => Self::Lower,
            "space" => Self::Space,
            "alpha" => Self::Alpha,
            "digit" => Self::Digit,
            "print" => Self::Print,
            "upper" => Self::Upper,
            "blank" => Self::Blank,
            "graph" => Self::Graph,
            "punct" => Self::Punct,
            "xdigit" => Self::Xdigit,
            _ => return Err(TakeError { begin: 42 }),
        })
    }
}

fn character_class(parser: &mut Parser) -> Result<CharacterClass, TakeError> {
    parser.try_parse(str_cmp("[:"))?;

    let mut word = String::new();

    loop {
        if parser.try_parse(str_cmp(":]")).is_ok() {
            break;
        }

        let c = parser.take()?;
        word.push(c);
    }

    CharacterClass::try_from(word.as_str())
}

fn bracket(parser: &mut Parser) -> Result<Bracket, TakeError> {
    let mut bracket = Bracket {
        insides: vec![],
        is_negative: false,
    };

    parser.try_parse(char_cmp('['))?;
    //if the first character in a bracket expression is ^, the bracket become negative
    // bracket.is_negative = parser.advance_if('^').is_some();
    bracket.is_negative = parser.try_parse(char_cmp('^')).is_ok();
    loop {
        //loop until closing bracket
        if parser.try_parse(char_cmp(']')).is_ok() {
            break;
        }
        if let Ok(range) = parser.try_parse(|p: &mut Parser<'_>| {
            let first = p.take()?;
            let middle = p.take()?;
            let end = p.take()?;

            if middle != '-' {
                return Err(TakeError { begin: 42 });
            }
            Ok(BracketInside::Range(first, end))
        }) {
            bracket.insides.push(range);
            continue;
        }

        if let Ok(char_class) = parser.try_parse(character_class) {
            bracket
                .insides
                .push(BracketInside::CharacterClass(char_class));
            continue;
        }

        if let Ok(c) = parser.try_parse(|p| p.take()) {
            bracket.insides.push(BracketInside::Single(c));
            continue;
        }

        return Err(TakeError { begin: usize::MAX });
    }
    Ok(bracket)
}

#[derive(Debug, Clone)]
pub enum Ast {
    Concatenate(Vec<Ast>),
    CharLiteral(char),
    Bracket(Bracket),
    Or(Box<Ast>, Box<Ast>),
    OneOrMore(Box<Ast>),
    NoneOrMore(Box<Ast>),
    Group(Box<Ast>),
}

impl Ast {
    pub fn simplify(&mut self) {
        match self {
            Ast::Concatenate(asts) => {
                if asts.len() == 1 {
                    let replacement = std::mem::replace(&mut asts[0], Ast::CharLiteral('\0'));
                    *self = replacement;
                }
            }
            Ast::CharLiteral(_) => {}
            Ast::Bracket(_) => {}
            Ast::Or(lhs, rhs) => {
                lhs.simplify();
                rhs.simplify();
            }
            Ast::OneOrMore(ast) => {
                ast.simplify();
            }
            Ast::NoneOrMore(ast) => {
                ast.simplify();
            }
            Ast::Group(ast) => {
                ast.simplify();
            }
        }
    }
}

pub fn char(parser: &mut Parser) -> Result<char, TakeError> {
    let result = parser.take();
    // if result.clone().is_ok_and(|c| c == '\\') {
    //     todo!("Implement \\ char, hexadecimal, octal, raw, ect");
    // }
    return result;
}

pub fn concatenate(parser: &mut Parser) -> Result<Ast, TakeError> {
    let mut nodes = vec![];
    while !parser.ended() {
        if parser.try_parse(char_cmp('|')).is_ok() {
            let lhs = Ast::Concatenate(nodes);
            let rhs = parser.try_parse(concatenate)?;
            let or = Ast::Or(Box::new(lhs), Box::new(rhs));
            return Ok(or);
        }

        if parser.try_parse(char_cmp('*')).is_ok() {
            let previous = Ast::Concatenate(nodes);
            let previous = Ast::NoneOrMore(Box::new(previous));
            nodes = vec![];
            nodes.push(previous);
            continue;
        }

        if parser.try_parse(char_cmp('+')).is_ok() {
            let previous = Ast::Concatenate(nodes);
            let previous = Ast::OneOrMore(Box::new(previous));
            nodes = vec![];
            nodes.push(previous);
            continue;
        }

        if parser.try_parse(char_cmp('(')).is_ok() {
            let expr = parser.try_parse(concatenate)?;
            parser.try_parse(char_cmp(')'))?;
            nodes.push(Ast::Group(Box::new(expr)));
            continue;
        }

        if parser.clone().try_parse(char_cmp(')')).is_ok() {
            break;
        }

        if let Ok(br) = parser.try_parse(bracket) {
            nodes.push(Ast::Bracket(br));
            continue;
        }

        if let Ok(c) = parser.try_parse(char) {
            nodes.push(Ast::CharLiteral(c));
            continue;
        }
    }
    Ok(Ast::Concatenate(nodes))
}

pub fn parse_ast(src: &str) -> Result<Ast, TakeError> {
    let v: Vec<char> = src.chars().collect();
    let mut parser = Parser::new(&v);
    let ast = parser.try_parse(concatenate);
    ast.and_then(|mut ast| {ast.simplify(); Ok(ast)})
}

// use std::{iter::Peekable, str::Chars};

// #[derive(Debug, Clone)]

// pub enum AstNode {
//     // --- Basic units ---
//     Literal(char), // e.g. 'a'
//     Dot,           // '.': match any single character
//     Escape(char),  // e.g. '\n', '\t', '\d', depending on your escape logic

//     // --- Character classes ---
//     Bracket(Box<Bracket>), // e.g. [a-z], [^a-z]
//     // --- Combinators ---
//     Concat(Vec<AstNode>),      // Concatenation of multiple nodes
//     Alternation(Vec<AstNode>), // a|b|c
//     Group(Box<AstNode>),       // ( ... ) -- for grouping/precedence

//     // --- Quantifiers ---
//     ZeroOrMore(Box<AstNode>),                   // a*
//     OneOrMore(Box<AstNode>),                    // a+
//     ZeroOrOne(Box<AstNode>),                    // a?
//     Repeat(Box<AstNode>, usize, Option<usize>), // a{m,n} or a{n}

//     // --- Anchors (Only seen at the begining and end of a regex rule) ---
//     StartOfLine, // ^
//     EndOfLine,   // $
// }

// #[derive(Debug, Clone)]
// pub struct Bracket {
//     pub negated: bool,          // true for [^a-z]
//     pub ranges: Vec<CharRange>, // e.g. 'a' to 'z'
//     pub singles: Vec<char>,     // specific characters like [abc]
// }

// #[derive(Debug, Clone)]
// pub struct CharRange {
//     pub start: char,
//     pub end: char,
// }

// pub struct Repeat {
//     expr: AstNode,
//     min: usize,
//     max: usize,
// }

// struct RegexParser<'a> {
//     src: Peekable<Chars<'a>>, //The source string from where you build your RegexAst
// }

// impl<'a> RegexParser<'a> {
//     fn new(source: &'a str) -> Self {
//         Self {
//             src: source.chars().peekable(),
//         }
//     }

//     fn parse_backslash(&mut self) -> Option<char> {
//         let special = self.src.next()?;

//         Some(match special {
//             'x' => {
//                 // Read every valid hexadecimal character after \x, and return the char represented by this value
//                 let mut nbr = 0;
//                 while let Some(hexa) = self.src.next_if(|c| c.is_digit(16)) {
//                     nbr = nbr * 16 + hexa.to_digit(16).unwrap();
//                 }
//                 char::from_u32(nbr)
//                     .expect("Escape sequence of hexadecimal digit, not able to make a valid char")
//             }
//             '0'..'7' => {
//                 let mut nbr = special.to_digit(8).unwrap();
//                 if let Some(octal) = self.src.next_if(|c| c.is_digit(8)) {
//                     nbr = nbr * 10 + octal.to_digit(8).unwrap();
//                 }
//                 if let Some(octal) = self.src.next_if(|c| c.is_digit(8)) {
//                     nbr = nbr * 10 + octal.to_digit(8).unwrap();
//                 }
//                 char::from_u32(nbr)
//                     .expect("Escape sequence of octal digit, not able to make a valid char")
//             }
//             '\\' => '\\',
//             'a' => '\x07',
//             'b' => '\x08',
//             't' => '\x09',
//             'n' => '\x0A',
//             'v' => '\x0B',
//             'f' => '\x0C',
//             'r' => '\x0D',
//             _ => special,
//         })
//     }

//     fn parse_literal(&mut self) -> Option<char> {
//         let c = self.src.next()?;
//         match c {
//             '\\' => self.parse_backslash(),
//             _ => Some(c),
//         }
//     }

//     fn parse_bracket(&mut self) -> Result<Bracket, ()> {
//         let mut bracket = Bracket {
//             negated: false,
//             ranges: vec![],
//             singles: vec![],
//         };

//         bracket.negated = self.src.next_if_eq(&'^').is_some();

//         while let Some(c) = self.src.next() {
//             match c {
//                 ']' => return Ok(bracket), //End of bracket
//                 '/' => {
//                     bracket.singles.push(
//                         self.parse_backslash()
//                             .expect("Expected character after backslash"),
//                     );
//                 }
//                 '-' => {
//                     let range = (bracket.singles.last(), self.parse_literal());
//                     match range {
//                         (Some(&start), Some(end)) => {
//                             bracket.ranges.push(CharRange {
//                                 start: start,
//                                 end: end,
//                             });
//                             bracket.singles.pop();
//                         }
//                         _ => {
//                             bracket.singles.push('-');
//                         }
//                     }
//                 }

//                 _ => bracket.singles.push(c),
//             }
//         }
//         return Err(());
//     }

//     fn parse_concat() -> AstNode {
//         let mut v: Vec<AstNode> = Vec::new();
//         // AstNode::Concat()
//         todo!()
//     }
// }
