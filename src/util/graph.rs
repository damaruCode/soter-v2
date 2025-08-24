use std::{collections::BTreeMap, usize};

pub trait NodeData: Eq + Clone + Ord {}
impl<T: Eq + Clone + Ord> NodeData for T {}

pub trait EdgeData: Eq + Clone {}
impl<T: Eq + Clone> EdgeData for T {}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Node<N: NodeData, E: EdgeData> {
    handle: usize,
    data: N,
    edges_to: Vec<ToEdge<E>>,
}

pub struct NodeAttributes {
    pub label: String,
    pub tooltip: String,
    pub group: String,
}
impl NodeAttributes {
    pub fn new() -> Self {
        Self {
            label: String::new(),
            tooltip: String::new(),
            group: String::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ToEdge<E: EdgeData> {
    node_handle: usize,
    data: E,
}
impl<E: EdgeData> ToEdge<E> {
    pub fn new(node_handle: usize, edge_data: E) -> Self {
        Self {
            node_handle,
            data: edge_data,
        }
    }
}

pub struct EdgeAttributes {
    pub label: String,
}
impl EdgeAttributes {
    pub fn new() -> Self {
        Self {
            label: String::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Graph<N: NodeData, E: EdgeData> {
    map: BTreeMap<N, Node<N, E>>,

    counter: usize,
}

impl<N: NodeData, E: EdgeData> Graph<N, E> {
    pub fn new() -> Self {
        Self {
            map: BTreeMap::new(),
            counter: 0,
        }
    }

    pub fn add_node(&mut self, node: N) -> usize {
        self.map.insert(
            node.clone(),
            Node {
                handle: self.counter,
                data: node,
                edges_to: Vec::new(),
            },
        );
        self.counter += 1;

        return self.counter - 1;
    }

    pub fn add_edge(&mut self, from: N, to: N, edge_data: E) {
        // check if to node exists
        let to_handle = match self.map.get(&to) {
            Some(node) => node.handle,
            None => self.add_node(to),
        };

        // check if from node already exists
        let from_node;
        match self.map.get_mut(&from) {
            Some(node) => from_node = node,
            None => {
                self.add_node(from.clone());
                from_node = self.map.get_mut(&from).unwrap()
            }
        }

        // only add to edge if it doesn't already exist
        let to_edge = ToEdge {
            node_handle: to_handle,
            data: edge_data,
        };
        if !from_node.edges_to.contains(&to_edge) {
            from_node.edges_to.push(to_edge);
        }
    }

    pub fn print_dot<F, G>(&self, format_node: F, format_edge: G) -> String
    where
        F: Fn(&N) -> NodeAttributes,
        G: Fn(&E) -> EdgeAttributes,
    {
        let mut dot_code = String::from("digraph {");
        for (_, node) in &self.map {
            let node_attr = format_node(&node.data);
            dot_code.push_str(
                format!(
                    "\t{}[label=\"{}\" shape=\"box\" tooltip=\"{}\", group=\"{}\"]\n",
                    node.handle, node_attr.label, node_attr.tooltip, node_attr.group
                )
                .as_str(),
            );
            for edge in &node.edges_to {
                let edge_attr = format_edge(&edge.data);
                dot_code.push_str(
                    format!(
                        "\t{} -> {} [label=\"{}\"]\n",
                        node.handle, edge.node_handle, edge_attr.label
                    )
                    .as_str(),
                );
            }
        }
        dot_code.push_str("}");

        dot_code
    }
}
