use crate::nfa::{Condition, Graph};
use std::str::Chars;
// To run through a non deterministic graph (nfa) we need to check each actions,
// but in a nfa the same actions can point to different state,
// so we make another "Worker" that will try to run along the different state,
// the last worker that finished running the graph and that is in a "accept" state, is the response we have to go through

#[derive(Debug, Clone, Copy)]
struct Worker {
    current_graph_node: usize,
    has_moved: bool,
}

pub struct Interpreter<'a> {
    workers: Vec<Worker>,
    // input: String,
    input: Chars<'a>,
    graph: Graph,
}

impl<'a> Interpreter<'a> {
    pub fn new(string: &'a str, graph: Graph) -> Self {
        Self {
            workers: vec![],
            input: string.chars(),
            graph: graph,
        }
    }

    fn char_match_condition(c: char, condition: &Condition) -> bool {
        match condition {
            // Condition::Epsilon => true,
            Condition::Single(single) => &c == single,
            Condition::Range(begin, end) => begin <= &c && end >= &c,
        }
    }

    // This advance a worker at the given index, in the graph.
    // To traverse all possibility (multiple edge can have the same condition in a nfa),
    // We duplicate the current worker and make them take the other possibilities
    fn worker_traverse(&mut self, mut worker_index: usize, current_char: char) {
        let mut edge_index = 0;
        let mut is_original = true;

        let worker_copy = Worker {
            current_graph_node: self.workers[worker_index].current_graph_node,
            has_moved: false,
        };

        let edges_count = self.graph.nodes[worker_copy.current_graph_node].edges.len();
        while edge_index < edges_count {
            let edge = &self.graph.nodes[worker_copy.current_graph_node].edges[edge_index];
            if Self::char_match_condition(current_char, &edge.condition) {
                //Flag to duplicate the original worker if multiple edges are possible
                if is_original {
                    is_original = false;
                } else {
                    self.workers.push(worker_copy);
                    worker_index = self.workers.len() - 1;
                }

                //We advance the worker to it's next position
                self.workers[worker_index].current_graph_node = edge.node;
                // self.workers[worker_index].has_moved = edge.condition != Condition::Epsilon;

                //Epsilon is a special case, it doesn't count as a real move so we need to call this function recursively
                // if edge.condition == Condition::Epsilon {
                //     self.worker_traverse(worker_index, current_char);
                // }
            }
            edge_index += 1;
        }
    }

    pub fn run(&mut self) -> Result<String, ()> {
        //Early return if no data to run

        //.0 is the index of the accepting state, .1 is the number of character traversed
        let mut accepted_state: Option<(usize, usize)> = None;
        let mut char_traversed_count: usize = 0;

        self.workers.clear();
        self.workers.push(Worker {
            current_graph_node: 0,
            has_moved: false,
        });
        let t = self.input.clone();
        for c in t {
            char_traversed_count += 1;
            let mut worker_i = 0;
            // Using while loop with index to push to the vector while iterating
            let workers_length = self.workers.len();
            while worker_i < workers_length {
                self.worker_traverse(worker_i, c);
                worker_i += 1;
            }

            //Look if any worker is in an accepting node, if so break the loop
            for worker in &self.workers {
                let accepting_state = self
                    .graph
                    .accept
                    .binary_search_by(|probe| probe.0.cmp(&worker.current_graph_node));
                if accepting_state.is_err() {
                    continue;
                }
                accepted_state = Some((accepting_state.unwrap(), char_traversed_count));
                break;
            }

            //We remove all workers that are flaged for destruction
            self.workers.retain(|worker| worker.has_moved);

            for worker in &mut self.workers {
                worker.has_moved = false;
            }
            // When no worker is active, we return the last accepted node (first for a given loop) triggered,
            // in practice this will give the largest valid regex output given a graph
            if self.workers.is_empty() {
                break;
            }
        }

        let mut yytext = String::new();

        match accepted_state {
            Some((accept_index, char_count)) => {
                dbg!(&self.graph.accept[accept_index].1);
                for _ in 0..char_count {
                    yytext.push(self.input.next().unwrap());
                }
                Ok(yytext)
            }
            None => {
                if char_traversed_count == 0 {
                    Err(())
                } else {
                    self.input.next();
                    Ok(yytext)
                }
            }
        }
    }
}
