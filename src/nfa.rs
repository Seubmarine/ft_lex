#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Condition {
    Epsilon,
    Single(char),
    Range(char, char),
}
impl Condition {
    //TODO:
    fn cmp(&self, condition: Condition) -> std::cmp::Ordering {
        match (self, condition) {
            (Condition::Epsilon, Condition::Epsilon) => std::cmp::Ordering::Equal,
            (Condition::Epsilon, Condition::Single(_)) => std::cmp::Ordering::Less,
            (Condition::Epsilon, Condition::Range(_, _)) => std::cmp::Ordering::Less,
            (Condition::Single(_), Condition::Epsilon) => std::cmp::Ordering::Greater,
            (Condition::Range(_, _), Condition::Epsilon) => std::cmp::Ordering::Greater,
            (Condition::Single(a), Condition::Single(b)) => a.cmp(&b),
            (Condition::Single(c), Condition::Range(a, b)) => {
                if c < &a {
                    std::cmp::Ordering::Less
                } else if c > &b {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Equal
                }
            }
            (Condition::Range(a, b), Condition::Single(c)) => {
                if &c < a {
                    std::cmp::Ordering::Less
                } else if &c > b {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Equal
                }
            }
            (Condition::Range(first_begin, _), Condition::Range(other_begin, _)) => {
                first_begin.cmp(&other_begin)
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Edge {
    pub condition: Condition,
    pub node: usize,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub edges: Vec<Edge>,
}

#[derive(Debug, Clone)]
pub struct Graph {
    pub nodes: Vec<Node>,
    pub accept: Vec<(usize, String)>,
}

#[derive(Debug, Clone, Copy)]
pub struct NodeId {
    inner: usize,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            nodes: vec![],
            accept: vec![],
        }
    }

    pub fn add_node(&mut self) -> NodeId {
        self.nodes.push(Node { edges: vec![] });
        let position = self.nodes.len() - 1;
        NodeId { inner: position }
    }

    pub fn node_accept(&mut self, id: NodeId, accept_action: String) {
        self.accept.push((id.inner, accept_action));
    }

    pub fn get_node(&mut self, id: NodeId) -> Option<&mut Node> {
        self.nodes.get_mut(id.inner)
    }

    pub fn add_edge(&mut self, from: NodeId, to: NodeId, condition: Condition) {
        match (self.nodes.get(to.inner), self.nodes.get(from.inner)) {
            (Some(_), Some(_)) => {}
            _ => {
                panic!("tried adding an edge between non existant node")
            }
        }

        let from_node = self.nodes.get_mut(from.inner).unwrap();
        from_node.edges.push(Edge {
            condition: condition,
            node: to.inner,
        });
    }

    pub fn compile(&mut self) {
        self.accept
            .sort_by(|(index_left, _), (index_right, _)| index_left.cmp(index_right));
    }

    pub fn dot_string(&self) -> String {
        let mut s = String::new();

        s.push_str("digraph G {\n");
        s.push_str("    rankdir=LR;\n");

        let accepting_nodes: Vec<String> = self
            .accept
            .iter()
            .map(|(idx, _)| *idx)
            .map(|n| n.to_string())
            .collect();
        let accepting_nodes = accepting_nodes.join(" ");
        let accepting_nodes = format!("    node [shape=doublecircle]; {};\n", accepting_nodes);
        s.push_str(&accepting_nodes);
        s.push_str("    node [shape=circle];");
        for (node_index, node) in self.nodes.iter().enumerate() {
            for edge in &node.edges {
                let edge_contion_str = match edge.condition {
                    Condition::Epsilon => "ε",
                    Condition::Single(c) => {
                        if c == '"' {
                            "\\\""
                        } else if c == '\\' {
                            "\\\\"
                        } else if c == ' ' {
                            "\' \'"
                        } else {
                            &c.to_string()
                        }
                    }
                    Condition::Range(begin, end) => &format!("{begin}-{end}"),
                };
                let line = format!(
                    "    {} -> {} [label=\"{}\"]\n",
                    node_index, edge.node, edge_contion_str
                );
                s.push_str(&line);
            }
        }
        s.push_str("}");
        s
    }
}
