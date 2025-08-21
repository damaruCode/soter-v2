use crate::state_space::KontinuationAddress;
use crate::state_space::ProcState;
use crate::state_space::ValueAddress;

use layout::adt::dag::NodeHandle;
use layout::backends::svg::SVGWriter;
use layout::core::base::Orientation;
use layout::core::color::Color;
use layout::core::geometry::Point;
use layout::core::style::*;
use layout::core::utils::save_to_file;
use layout::std_shapes::shapes::*;
use layout::topo::layout::VisualGraph;

use std::collections::BTreeMap;

pub struct GraphBuilder<K: KontinuationAddress, V: ValueAddress> {
    graph: VisualGraph,
    style: StyleAttr,
    lookup: BTreeMap<ProcState<K, V>, NodeHandle>,
}

impl<K: KontinuationAddress, V: ValueAddress> GraphBuilder<K, V> {
    pub fn new() -> Self {
        let graph = VisualGraph::new(Orientation::TopToBottom);
        let style = StyleAttr::simple();

        GraphBuilder {
            graph,
            style,

            lookup: BTreeMap::new(),
        }
    }

    pub fn add_node(&mut self, node: ProcState<K, V>) {
        let shape = ShapeKind::new_box(&format!(
            "{}\n{}\n{}\n{}\n{}",
            node.pid, node.prog_loc_or_pid, node.env, node.k_addr, node.time,
        ));

        let mut sizes = Vec::new();
        sizes.push(format!("{}", node.pid).len());
        sizes.push(format!("{}", node.prog_loc_or_pid).len());
        sizes.push(format!("{}", node.env).len());
        sizes.push(format!("{}", node.k_addr).len());
        sizes.push(format!("{}", node.time).len());
        sizes.sort();
        let max = sizes.last().unwrap();

        let handle = self.graph.add_node(Element::create(
            shape,
            self.style.clone(),
            Orientation::LeftToRight,
            Point::new(125. + *max as f64 * 5., 125.),
        ));

        self.lookup.insert(node, handle);
    }

    pub fn add_edge(&mut self, start: ProcState<K, V>, end: ProcState<K, V>, name: &str) {
        let arrow = Arrow::new(
            LineEndKind::None,
            LineEndKind::Arrow,
            LineStyleKind::Dotted,
            name,
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
        self.graph.add_edge(
            arrow,
            *self.lookup.get(&start).unwrap(),
            *self.lookup.get(&end).unwrap(),
        );
    }

    pub fn print(&mut self, path: &str) {
        // Render the nodes to some rendering backend.
        let mut svg = SVGWriter::new();
        self.graph.do_it(false, false, false, &mut svg);

        // Save the output.
        let _ = save_to_file(path, &svg.finalize());
    }
}
