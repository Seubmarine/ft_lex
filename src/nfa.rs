
#[derive(PartialEq, Eq, Clone, Copy)]
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
            (Condition::Epsilon, Condition::Range(_, _)) => todo!(),
            (Condition::Single(_), Condition::Epsilon) => std::cmp::Ordering::Greater,
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
            (Condition::Range(_, _), Condition::Epsilon) => todo!(),
            (Condition::Range(a, b), Condition::Single(c)) => {
                if &c < a {
                    std::cmp::Ordering::Less
                } else if &c > b {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Equal
                }
            }
            (Condition::Range(_, _), Condition::Range(_, _)) => todo!(),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct Edge {
    pub condition: Condition,
    pub node: usize,
}

pub struct Node {
    pub edges: Vec<Edge>,
}

pub struct Graph {
    pub nodes: Vec<Node>,
    pub accept: Vec<(usize, String)>,
}

#[derive(Debug, Clone, Copy)]
pub struct NodeId {
    inner: usize,
}

impl Graph {
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
        for node in &mut self.nodes {
            node.edges
                .sort_by(|edge1, edge2| edge1.condition.cmp(edge2.condition));
        }
    }
}
