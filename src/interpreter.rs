
use crate::nfa::{Condition, Graph};
use std::str::Chars;
// To run through a non deterministic graph (nfa) we need to check each actions,
// but in a nfa the same actions can point to different state,
// so we make another "Worker" that will try to run along the different state,
// the last worker that finished running the graph and that is in a "accept" state, is the response we have to go through

struct Worker {
    current_graph_node: usize,
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
            Condition::Epsilon => true,
            Condition::Single(single) => &c == single,
            Condition::Range(begin, end) => begin <= &c && end >= &c,
        }
    }

    pub fn run(&mut self) -> Result<String, ()> {
        
        //Early return if no data to run
        
        //.0 is the index of the accepting state, .1 is the number of character traversed
        let mut accepted_state: Option<(usize, usize)> = None;
        let mut char_traversed_count: usize = 0;
        
        self.workers.clear();
        self.workers.push(Worker { current_graph_node: 0 });
        let t = self.input.clone();
        for c in t {
            char_traversed_count += 1;
            let mut worker_i = 0;
            // Using while loop with index to push to the vector while iterating
            let workers_length = self.workers.len();
            while worker_i < workers_length {
                
                let worker = &self.workers[worker_i];

                let node_current_worker = &self.graph.nodes[worker.current_graph_node];
                let mut has_moved = false;

                //We separate each edge by matching condition,
                // [ [Edge {'a', Node0}, Edge{'a', Node1}] , [Edge{'b', Node0}] , [Edge{'c', Node2}, Edge{'c', Node1}] ]
                let edges_chunked = node_current_worker
                    .edges
                    .chunk_by(|a, b| a.condition == b.condition);
                for edge_chunk in edges_chunked {
                    //We only need to check condition of the first one, since each edge_chunk are grouped by same condition
                    let condition = &edge_chunk[0].condition;
                    if !Self::char_match_condition(c, condition) {
                        continue;
                    }

                    let mut edge_iterator = edge_chunk.into_iter();
                    self.workers[worker_i].current_graph_node = edge_iterator.next().unwrap().node;
                    has_moved = true;
                    //If there's other edge with the same condition we create other workers that will take those edge
                    for remaining_edge in edge_iterator {
                        self.workers.push(Worker {
                            current_graph_node: remaining_edge.node,
                        });
                    }
                    break;
                }

                //If the current worker is stuck we flag it for destruction by setting it's node position to usize::MAX
                if !has_moved {
                    self.workers[worker_i].current_graph_node = usize::MAX;
                }
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
                // let accepting_state = accepting_state.unwrap();
                // let accepting_state = &self.graph.accept[accepting_state].1;
                break;
            }

            //We remove all workers that are flaged for destruction
            self.workers
                .retain(|worker| worker.current_graph_node != usize::MAX);

            // When no worker is active, we return the last accepted node (first for a given loop) triggered,
            // in practice this will give the largest valid regex output given a graph
            if self.workers.is_empty() {
                break;
            }
        }

        let mut yytext = String::new();

        match accepted_state {
            Some((accept_index,char_count)) => {
                dbg!(&self.graph.accept[accept_index].1);
                for _ in 0..char_count {
                    yytext.push(self.input.next().unwrap());
                } 
                Ok(yytext)
            },
            None => {
                if char_traversed_count == 0 {
                    Err(())
                } else {
                    self.input.next();
                    Ok(yytext)
                }
            },
        }


    }
}
