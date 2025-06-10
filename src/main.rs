pub mod interpreter;
pub mod nfa;
pub mod parser;
pub mod regex_parser;

use std::{
    env::args,
    fs::{self, File},
    io::{BufWriter, Read, Write},
    process::Command,
    str::{self, Chars},
};

use interpreter::Interpreter;
use nfa::{Graph, NodeId};
use parser::Parser;
use regex_parser::{concatenate, parse_ast};

use crate::regex_parser::Provenance;

pub mod lex {

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub struct NameSubstitute<'a> {
        pub name: &'a str,
        pub substitutes: &'a str,
    }

    #[derive(Debug, Clone)]
    pub struct Rule<'a> {
        pub regex: &'a str,
        pub action: &'a str,
    }

    #[derive(Debug, Clone)]
    pub struct Config<'a> {
        // user_startroutines : String,
        // definition: (String, String),
        pub substitutes: Vec<NameSubstitute<'a>>, //LexRule<'a>,
        pub rules: Vec<Rule<'a>>,
        pub user_subroutines: Option<&'a str>,
    }

    impl<'a> Config<'a> {
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
                    self.substitutes.push(NameSubstitute {
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

        pub fn new(config: &'a str) -> Self {
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
}

fn main() -> std::io::Result<()> {
    let filename: &str = &args()
        .nth(1)
        .unwrap_or("examples/one_or_more.l".to_string());
    let mut f = File::open(filename)?;
    let mut data = vec![];
    f.read_to_end(&mut data)?;
    let file_string = str::from_utf8(&data).unwrap();

    let config = lex::Config::new(file_string);

    let mut graph = Graph::new();
    let graph_entry = graph.add_node();

    for rule in &config.rules {
        let ast = parse_ast(rule.regex, &config.substitutes);
        match ast {
            Ok(ast) => {
                println!("rule named {} got valid AST: {:?}", rule.regex, ast);
                println!("");

                // let rule_entry = graph.add_node();

                let mut p = Provenance::new(graph_entry);
                ast.ast_connect_graph(&mut graph, &mut p);
                p.advance_to_empty_node(&mut graph);
            }
            Err(err) => {
                println!("rule named {} got error at: {}", rule.regex, err.begin);
            }
        }
    }

    graph.compile();

    // dbg!(&graph);

    let dot_string = graph.dot_string();
    fs::write("graph.dot", dot_string)?;
    Command::new("dot")
        .args(&["-Tpng", "graph.dot", "-o", "output.png"])
        .spawn()
        .unwrap();
    Ok(())
}
