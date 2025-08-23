use std::collections::BTreeMap;

use layout::{
    backends::svg::SVGWriter,
    core::{
        base::Orientation,
        color::Color,
        geometry::Point,
        style::{LineStyleKind, StyleAttr},
        utils::save_to_file,
    },
    std_shapes::shapes::{Arrow, Element, LineEndKind, ShapeKind},
    topo::layout::VisualGraph,
};

pub trait NodeData: Eq + Clone + Ord {}
impl<T: Eq + Clone + Ord> NodeData for T {}

pub trait EdgeData: Eq + Clone {}
impl<T: Eq + Clone> EdgeData for T {}

#[derive(Debug)]
pub enum GraphError<N: NodeData> {
    MissingNode(Node<N>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Node<N: NodeData> {
    data: N,
}
impl<N: NodeData> Node<N> {
    pub fn new(data: N) -> Self {
        Self { data }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToEdge<N: NodeData, E: EdgeData> {
    node: Node<N>,
    data: E,
}
impl<N: NodeData, E: EdgeData> ToEdge<N, E> {
    pub fn new(node: Node<N>, edge_data: E) -> Self {
        Self {
            node,
            data: edge_data,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Graph<N: NodeData, E: EdgeData> {
    map: BTreeMap<Node<N>, Vec<ToEdge<N, E>>>,
}

impl<N: NodeData, E: EdgeData> Graph<N, E> {
    pub fn new() -> Self {
        Self {
            map: BTreeMap::new(),
        }
    }

    pub fn add_node(&mut self, data: N) {
        self.map.insert(Node::new(data), Vec::new());
    }

    pub fn add_edge(
        &mut self,
        from: Node<N>,
        to: Node<N>,
        edge_data: E,
    ) -> Result<(), GraphError<N>> {
        // check if to node exists
        if !self.map.contains_key(&to) {
            return Err(GraphError::MissingNode(to));
        }

        // check if from node already exists
        let edges;
        match self.map.get_mut(&from) {
            Some(vec) => edges = vec,
            None => return Err(GraphError::MissingNode(from)),
        }

        // only add to edge if it doesn't already exist
        let to_edge = ToEdge::new(to, edge_data);
        if !edges.contains(&to_edge) {
            edges.push(to_edge);
        }

        Ok(())
    }

    pub fn export<F, G>(&self, export_path: &str, format_node: F, format_edge: G)
    where
        F: Fn(N) -> String,
        G: Fn(E) -> String,
    {
        let mut visual_graph = VisualGraph::new(Orientation::TopToBottom);
        let mut lookup = BTreeMap::new();
        let style = StyleAttr::simple();

        for (node, _) in &self.map {
            let node_content = format_node(node.data.clone());
            let shape = ShapeKind::new_box(node_content.as_str());

            let mut sizes = Vec::new();
            for row in node_content.split("\n") {
                sizes.push(row.len());
            }
            sizes.sort();
            let max = sizes.last().unwrap();

            let handle = visual_graph.add_node(Element::create(
                shape,
                style.clone(),
                Orientation::LeftToRight,
                Point::new(125. + *max as f64 * 5., 125.),
            ));

            lookup.insert(node.clone(), handle);
        }

        for (from_node, edges) in &self.map {
            for edge in edges {
                let arrow = Arrow::new(
                    LineEndKind::None,
                    LineEndKind::Arrow,
                    LineStyleKind::Dotted,
                    format_edge(edge.data.clone()).as_str(),
                    &StyleAttr::new(
                        Color::fast("black"),
                        2,
                        Option::Some(Color::fast("white")),
                        2,
                        20,
                    ),
                    &None,
                    &None,
                );

                visual_graph.add_edge(
                    arrow,
                    *lookup.get(&from_node).unwrap(),
                    *lookup.get(&edge.node).unwrap(),
                );
            }
        }

        // Render the nodes to some rendering backend.
        let mut svg = SVGWriter::new();
        visual_graph.do_it(false, false, false, &mut svg);

        // Save the output.
        let _ = save_to_file(export_path, &svg.finalize());
    }
}
