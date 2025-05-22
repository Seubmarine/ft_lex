use std::str::Chars;

#[derive(Clone)]
struct Rule<'a> {
    name: &'a str,
    rule: Chars<'a>,
}

impl<'a> Rule<'a> {
    fn new(name: &'a str, rule: &'a str) -> Self {
        Self {
            name,
            rule: rule.chars(),
        }
    }
}

//This need to be implemented directly into the parser, but the current implementation doesn't implement Copy
struct RegexIter<'a> {
    stack: Vec<Rule<'a>>,
    rules: &'a [Rule<'a>],
}

impl<'a> RegexIter<'a> {
    fn new(regex: &'a str, rules: &'a [Rule]) -> Self {
        Self {
            stack: vec![Rule::new("", regex)],
            rules,
        }
    }
}

// This return a &str of the string between begin and end,
// it return None if not found or if begin is not at index 0 from src
fn next_str_is_between(src: &str, begin: char, end: char) -> Option<&str> {
    let begin = src.find(begin);
    if let Some(begin) = begin {
        if begin != 0 {
            return None;
        }
    }
    let end = src.find(end);
    match (begin, end) {
        (Some(begin), Some(end)) => {
            if begin > end {
                None
            } else {
                Some(&src[begin + 1..end])
            }
        }
        _ => None,
    }
}

impl Iterator for RegexIter<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.stack.last_mut() {
            let regex = &mut current.rule;
            // When we uncounter a rule we push it to the stack and return '('
            // so we finish reading the rule we remove it and return ')' to close it
            // the first element in the stack is exempt of this rule since it's the original regex rule
            if regex.as_str().is_empty() {
                self.stack.pop();
                if !self.stack.is_empty() {
                    return Some(')');
                }
                return None;
            }

            //Looking for rule_name between '{' and '}'
            let rule_name: Option<&str> = next_str_is_between(regex.as_str(), '{', '}');
            if let Some(rule_name) = rule_name {
                let rule_referenced = self.rules.iter().find(|&rule| rule.name == rule_name);
                //
                if rule_referenced.is_none() {
                    return regex.next();
                }
                let rule_referenced = rule_referenced.unwrap();

                assert!(regex.nth(rule_name.len() + 1) == Some('}'));
                self.stack.push(rule_referenced.to_owned());
                // If this is trigered this means that a rule recursively calls itself, and this regex rules is impossible to satisfy
                // stack == [A, B, C, A]
                if self.stack[..self.stack.len() - 1]
                    .iter()
                    .any(|past_rule| past_rule.name == rule_name)
                {
                    let rule_stack = self.stack.iter().skip(1).rev().map(|rule| rule.name);
                    let rule_stack: Vec<&str> = rule_stack.collect();
                    let rule_stack = rule_stack.join(", ");

                    let error_message = format!(
                        "Subsitutes {} recursively calls itself, from rules [{}]",
                        rule_name, rule_stack
                    );
                    println!("{error_message}");
                    todo!("Exit gracefully on recursive substitutes")
                }
                return Some('(');
            }
            regex.next()
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TakeError {
    pub begin: usize,
}

#[derive(Clone, Copy)]
pub struct Parser<'a> {
    src: &'a [char],
    pub cursor: usize,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a [char]) -> Self {
        Self { src, cursor: 0 }
    }

    pub fn take(&mut self) -> Result<char, TakeError> {
        let value = self
            .src
            .get(self.cursor)
            .and_then(|c| Some(*c))
            .ok_or(TakeError { begin: self.cursor });
        self.cursor += 1;
        value
    }

    pub fn ended(&self) -> bool {
        self.src.len() <= self.cursor
    }

    pub fn peek(&self) -> Option<char> {
        self.src.get(self.cursor).copied()
    }

    pub fn try_parse<T, E>(&mut self, func: impl Fn(&mut Parser) -> Result<T, E>) -> Result<T, E> {
        let clone = &mut self.clone();
        let result = func(clone);
        if result.is_ok() {
            self.cursor = clone.cursor;
        }
        result
    }
}
