use core::slice;
use std::{
    cmp::Ordering,
    iter,
    slice::{Iter, IterMut},
    thread::current,
};

use crate::{
    lex,
    nfa::{Condition, Graph, NodeId},
    parser::{Parser, TakeError},
};

//This is the fuction used to parse any lex regex expression into a valid AST
pub fn parse_ast(
    src: &str,
    name_substitutes_list: &[lex::NameSubstitute<'_>],
) -> Result<Ast, TakeError> {
    let mut parser = Parser::new(src, name_substitutes_list);
    let ast = parser.try_parse(concatenate);
    ast
}

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
                begin: parser.cursor,
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
    OneOrMore(Box<Ast>),  //+
    NoneOrMore(Box<Ast>), //*
    Group(Box<Ast>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub struct ProvenanceData {
    origin: NodeId,
    condition: Option<Condition>,
}

impl Ord for ProvenanceData {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let c = self.origin.cmp(&other.origin);
        if c == Ordering::Equal {
            match (self.condition, other.condition) {
                (None, None) => return Ordering::Equal,
                (None, Some(_)) => return Ordering::Less,
                (Some(_), None) => return Ordering::Greater,
                (Some(left), Some(right)) => {
                    return left.cmp(&right);
                }
            }
        }
        c
    }
}

#[derive(Debug, Clone)]
pub struct Provenance {
    input: Vec<ProvenanceData>,
}

impl Provenance {
    pub fn new(node: NodeId) -> Self {
        Self {
            input: vec![ProvenanceData {
                origin: node,
                condition: None,
            }],
        }
    }
}

impl Provenance {
    pub fn extend(&mut self, other: &Provenance) {
        self.input.extend(other);
    }

    const fn as_mut_slice<'a>(&'a mut self) -> &'a mut [ProvenanceData] {
        self.input.as_mut_slice()
    }

    const fn as_slice<'a>(&'a self) -> &'a [ProvenanceData] {
        self.input.as_slice()
    }

    pub fn advance_to_empty_node(&mut self, graph: &mut Graph) {
        if !self.as_slice().iter().any(|p| p.condition.is_some()) {
            return;
        }

        let node = graph.add_node();

        for p in self.as_mut_slice() {
            if let Some(condition) = p.condition {
                graph.add_edge(p.origin, node, condition);
                p.origin = node;
                p.condition = None;
            }
        }
    }

    pub fn connect(&mut self, graph: &mut Graph, next_condition: Condition) {
        self.input.sort();
        self.input.dedup();
        for p in self.as_mut_slice() {
            match p.condition {
                Some(condition) => {
                    let next_node = graph.add_node();
                    graph.add_edge(p.origin, next_node, condition);
                    p.condition = Some(next_condition);
                    p.origin = next_node;
                }
                None => {
                    p.condition = Some(next_condition);
                }
            }
        }
    }
}

impl<'a> IntoIterator for &'a Provenance {
    type Item = &'a ProvenanceData;
    type IntoIter = std::slice::Iter<'a, ProvenanceData>;

    fn into_iter(self) -> Self::IntoIter {
        self.as_slice().iter()
    }
}

impl<'a> IntoIterator for &'a mut Provenance {
    type Item = &'a mut ProvenanceData;
    type IntoIter = std::slice::IterMut<'a, ProvenanceData>;

    fn into_iter(self) -> Self::IntoIter {
        self.as_mut_slice().iter_mut()
    }
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

    //Return the next node
    pub fn ast_connect_graph(&self, graph: &mut Graph, provenance: &mut Provenance) {
        match self {
            Ast::Concatenate(asts) => {
                for ast in asts {
                    ast.ast_connect_graph(graph, provenance);
                }
            }
            Ast::CharLiteral(c) => {
                provenance.connect(graph, Condition::Single(*c));
            }
            Ast::Bracket(bracket) => {
                todo!();
            }
            Ast::Or(ast_left, ast_right) => {
                let mut p1 = provenance.clone();

                ast_left.ast_connect_graph(graph, &mut p1);

                let p2 = provenance;
                ast_right.ast_connect_graph(graph, p2);

                p2.extend(&p1);
            }
            Ast::OneOrMore(ast) => {
                ast.ast_connect_graph(graph, provenance);

                provenance.advance_to_empty_node(graph);
                let current = provenance.clone();

                ast.ast_connect_graph(graph, provenance);

                for p in current.as_slice() {
                    for end in provenance.as_mut_slice() {
                        if let Some(end_condition) = end.condition {
                            graph.add_edge(end.origin, p.origin, end_condition);
                            end.origin = p.origin;
                            end.condition = None;
                        }
                    }
                }

                provenance.input.sort();
                provenance.input.dedup();
            }
            Ast::NoneOrMore(ast) => {
                provenance.advance_to_empty_node(graph);
                let current = provenance.clone();

                ast.ast_connect_graph(graph, provenance);

                for p in current.as_slice() {
                    for end in provenance.as_mut_slice() {
                        if let Some(end_condition) = end.condition {
                            graph.add_edge(end.origin, p.origin, end_condition);
                            end.origin = p.origin;
                            end.condition = None;
                        }
                    }
                }

                provenance.input.sort();
                provenance.input.dedup();

                // *provenance = current;
                // dbg!(&current, &provenance);
                // let next = ast.ast_connect_graph(graph, begin_node);
                // graph.add_edge(next, begin_node, Condition::Epsilon);
                // return begin_node;
                // let node_final = ast.ast_connect_graph(graph, begin_node, Some(begin_node));

                // ast.ast_connect_graph(graph, begin_node, end_node);
                // if end_node.is_some() {
                //     return end_node.unwrap();
                // }
                // return begin_node;
            }
            Ast::Group(ast) => {
                ast.ast_connect_graph(graph, provenance);
                // todo!();
                // return ast.ast_connect_graph(graph, begin_node, end_node);
            }
        };
    }
}

fn take_radix(radix: u32) -> impl Fn(&mut Parser) -> Result<u32, TakeError> {
    move |parser: &mut Parser<'_>| {
        let d = parser.take()?;
        d.to_digit(radix).ok_or(TakeError {
            begin: parser.cursor,
        })
    }
}

pub fn char(parser: &mut Parser) -> Result<char, TakeError> {
    let c = parser.take()?;
    if c != '\\' {
        return Ok(c);
    }

    //Backslash was found, we need to check for special character
    let special = parser.take()?;

    Ok(match special {
        'x' => {
            // Read every valid hexadecimal character after \x, and return the char represented by this value
            let mut nbr = 0;

            while let Ok(hexa) = parser.try_parse(take_radix(16)) {
                nbr = nbr * 16 + hexa;
            }
            if nbr == 0 {
                // \xdigits expect hexadecimal digits and that they are not equal to NUL");
                return Err(TakeError {
                    begin: parser.cursor,
                });
            }
            char::from_u32(nbr)
                .expect("Escape sequence of hexadecimal digit, not able to make a valid char")
        }
        '0'..'7' => {
            let mut nbr = special.to_digit(8).unwrap();

            if let Ok(octal) = parser.try_parse(take_radix(8)) {
                nbr = nbr * 10 + octal;
            }
            if let Ok(octal) = parser.try_parse(take_radix(8)) {
                nbr = nbr * 10 + octal;
            }

            if nbr == 0 {
                // \digits expect octal digits and that they are not equal to NUL"
                return Err(TakeError {
                    begin: parser.cursor,
                });
            }
            char::from_u32(nbr)
                .expect("Escape sequence of octal digit, not able to make a valid char")
        }
        '\\' => '\\',
        'a' => '\x07',
        'b' => '\x08',
        't' => '\x09',
        'n' => '\x0A',
        'v' => '\x0B',
        'f' => '\x0C',
        'r' => '\x0D',
        _ => special,
    })
}

fn name_substitution(parser: &mut Parser) -> Result<Ast, TakeError> {
    parser.try_parse(char_cmp('{'))?;

    let mut name_to_find = String::new();
    while let Ok(char) = parser.take() {
        if char == '}' {
            break;
        }
        name_to_find.push(char);
    }

    let name_substitute = parser
        .name_substitutions
        .iter()
        .find(|&ns| ns.name == name_to_find);

    match name_substitute {
        None => {
            eprintln!("undefined definition {{{name_to_find}}}");
            Err(TakeError {
                begin: parser.cursor,
            })
        }
        Some(name_substitute) => {
            if parser
                .name_substitution_stack
                .iter()
                .find(|&&previous_ns| previous_ns == name_substitute)
                .is_some()
            {
                eprintln!("recursive definition {{{name_to_find}}}");
                Err(TakeError {
                    begin: parser.cursor,
                })
            } else {
                parser.name_substitution_stack.push(name_substitute);

                let previous_src = parser.src;
                let previous_cursor_position = parser.cursor;

                parser.src = name_substitute.substitutes;
                parser.cursor = 0;
                let ast = parser.try_parse(concatenate);

                parser.src = previous_src;
                parser.cursor = previous_cursor_position;

                parser.name_substitution_stack.pop();
                let ast = ast?;
                Ok(Ast::Group(Box::new(ast)))
            }
        }
    }
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
            let previous = nodes.pop().expect("* should have a token to it's left");
            let previous = Ast::NoneOrMore(Box::new(previous));
            nodes.push(previous);
            continue;
        }

        if parser.try_parse(char_cmp('+')).is_ok() {
            let previous = nodes.pop().expect("+ should have a token to it's left");
            let previous = Ast::OneOrMore(Box::new(previous));
            nodes.push(previous);
            continue;
        }

        if let Ok(ast) = parser.try_parse(name_substitution) {
            nodes.push(ast);
            continue;
        }

        if parser.try_parse(char_cmp('(')).is_ok() {
            let expr = parser.try_parse(concatenate)?;
            parser.try_parse(char_cmp(')'))?;
            nodes.push(Ast::Group(Box::new(expr)));
            continue;
        }

        //TODO: remove clone
        if parser.clone().try_parse(char_cmp(')')).is_ok() {
            break;
        }

        if let Ok(br) = parser.try_parse(bracket) {
            nodes.push(Ast::Bracket(br));
            continue;
        }

        let c = parser.try_parse(char)?;
        nodes.push(Ast::CharLiteral(c));
    }
    if nodes.len() == 1 {
        let first = nodes.swap_remove(0);
        drop(nodes);
        dbg!("nodes length == 1", &first);
        Ok(first)
    } else {
        Ok(Ast::Concatenate(nodes))
    }
}
