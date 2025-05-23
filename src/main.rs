pub mod interpreter;
pub mod nfa;
pub mod parser;
pub mod regex_parser;

use std::{
    env::args,
    fs::{self, File},
    io::{BufWriter, Read, Write},
    str::{self, Chars},
};

use interpreter::Interpreter;
use nfa::{Graph, NodeId};
use parser::Parser;
use regex_parser::{concatenate, parse_ast};

#[derive(Debug, Clone, Copy)]
struct LexSubstitute<'a> {
    name: &'a str,
    substitutes: &'a str,
}

#[derive(Debug, Clone)]
struct Rule<'a> {
    regex: &'a str,
    action: &'a str,
}

struct RegexIterator<'a> {
    regex: Chars<'a>,
    substitutes_list: &'a Vec<LexSubstitute<'a>>,
    substitutes_stack: Vec<(LexSubstitute<'a>, RegexIterator<'a>)>,
}

impl<'a> RegexIterator<'a> {
    fn new(regex: &'a str, substitutes_list: &'a Vec<LexSubstitute<'a>>) -> Self {
        Self {
            regex: regex.chars(),
            substitutes_list,
            substitutes_stack: vec![],
        }
    }
}

impl<'a> Iterator for RegexIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((lex_substite, substitute_iterator)) = self.substitutes_stack.last_mut() {
            let substite_c = substitute_iterator.next();
            if substite_c.is_none() {}
        }

        let n = self.next();
        if let Some(c) = n {}
        n
    }
}

#[derive(Debug, Clone)]
struct LexConfig<'a> {
    // user_startroutines : String,
    // definition: (String, String),
    substitutes: Vec<LexSubstitute<'a>>, //LexRule<'a>,
    rules: Vec<Rule<'a>>,
    user_subroutines: Option<&'a str>,
}

impl<'a> LexConfig<'a> {
    fn parse_definitions(&mut self, definition_section: &'a str) {
        let lines = definition_section.lines();

        let mut start_subroutines: String = String::new();

        let mut need_closing_parenthesis = false;
        for line in lines {
            // if line.is_empty() {
            //     continue;
            // }

            if let Some(begin) = line.get(0..2) {
                if !need_closing_parenthesis && begin == "%{" {
                    need_closing_parenthesis = true;
                    continue;
                }
                if need_closing_parenthesis && begin == "%}" {
                    need_closing_parenthesis = false;
                    continue;
                }

                if need_closing_parenthesis {
                    start_subroutines.push_str(line);
                    start_subroutines.push_str("\n");
                    continue;
                }
            }

            if let Some(first_char) = line.get(..1) {
                if first_char == " " || first_char == "\t" {
                    start_subroutines.push_str(line);
                    start_subroutines.push_str("\n");
                    continue;
                }
            }

            if line.is_empty() {
                continue;
            }
            //Split the string in two from the first set of whitespace uncountered
            if let Some(begin_whitespace_position) = line.find(|x: char| x.is_whitespace()) {
                let substitute_name = &line[..begin_whitespace_position];

                let substitute_string = line[begin_whitespace_position..].trim();
                self.substitutes.push(LexSubstitute {
                    name: substitute_name,
                    substitutes: substitute_string,
                });
            }
        }
        println!("{start_subroutines}");
    }

    fn parse_rules(&mut self, sections: &'a str) {
        let lines = sections.lines();

        for line in lines {
            let mut inside_quote = false;
            let mut inside_brackets = false;

            let mut chars = line.chars().enumerate();
            while let Some((i, c)) = chars.next() {
                match c {
                    '\\' => {
                        chars.next();
                    }
                    '[' if !inside_brackets => {
                        inside_brackets = true;
                    }
                    ']' if inside_brackets => {
                        inside_brackets = false;
                    }
                    '\"' => inside_quote = !inside_quote,
                    ' ' | '\t' if !inside_brackets && !inside_quote => {
                        let regex = &line[..i];
                        if regex.is_empty() {
                            break;
                        }
                        self.rules.push(Rule { regex, action: "" });
                        dbg!(regex);
                        break;
                    }
                    _ => (),
                }
            }
        }
    }

    fn new(config: &'a str) -> Self {
        let mut sections = config.split("%%");

        let definition_section = sections.next().expect("Missing %%");
        let rules_section = sections.next().unwrap();
        let user_subroutines = sections.next();
        let mut s = Self {
            user_subroutines,
            rules: Vec::new(),
            substitutes: Vec::new(),
        };
        s.parse_definitions(definition_section);
        s.parse_rules(rules_section);
        s
    }
}

fn main() -> std::io::Result<()> {
    let filename: &str = &args().nth(1).unwrap_or("examples/spec.l".to_string());
    let mut f = File::open(filename)?;
    let mut data = vec![];
    f.read_to_end(&mut data)?;
    let file_string = str::from_utf8(&data).unwrap();

    let config = LexConfig::new(file_string);

    let mut graph = Graph::new();
    let begin_node = graph.add_node();

    for rule in &config.rules {
        let ast = parse_ast(rule.regex);
        match ast {
            Ok(ast) => {
                println!("rule named {} got valid AST: {:?}", rule.regex, ast);
                println!("");
                let end_node = ast.ast_connect_graph(&mut graph, begin_node);
                graph.node_accept(end_node, rule.regex.into());
            }
            Err(err) => {
                println!("rule named {} got error at: {}", rule.regex, err.begin);
            }
        }
    }

    graph.compile();

    let dot_string = graph.dot_string();
    fs::write("graph.dot", dot_string)?;

    println!("{:?}\n", graph.nodes);

    let mut interpreter = Interpreter::new("hello world if zz", graph);
    while let Ok(s) = interpreter.run() {
        println!("string runned: {s}|");
    }

    println!("{:?}", config);

    let mut graph = nfa::Graph {
        nodes: vec![],
        accept: vec![],
    };
    let a1 = graph.add_node();
    let a_recursive = graph.add_node();
    let b1 = graph.add_node();

    graph.add_edge(a1, a_recursive, nfa::Condition::Single('a'));
    graph.add_edge(a_recursive, a_recursive, nfa::Condition::Single('a'));
    graph.add_edge(a_recursive, b1, nfa::Condition::Single('b'));
    graph.node_accept(b1, "a+b".into());

    let number = graph.add_node();
    graph.add_edge(a1, number, nfa::Condition::Range('0', '9'));
    graph.add_edge(number, number, nfa::Condition::Range('0', '9'));
    graph.node_accept(number, "integer".into());

    let point = graph.add_node();
    graph.add_edge(number, point, nfa::Condition::Single('.'));
    graph.add_edge(point, point, nfa::Condition::Range('0', '9'));
    graph.node_accept(point, "float".into());

    graph.compile();
    let mut interpreter = Interpreter::new("ab   abc42.28b41aab539784", graph);
    while let Ok(s) = interpreter.run() {
        println!("string runned: {s}|");
    }
    Ok(())
}
