use crate::state_space::KontinuationAddress;
use crate::state_space::ProcState;
use crate::state_space::ValueAddress;

use layout::adt::dag::NodeHandle;
use layout::backends::svg::SVGWriter;
use layout::core::base::Orientation;
use layout::core::geometry::Point;
use layout::core::style::*;
use layout::core::utils::save_to_file;
use layout::std_shapes::shapes::*;
use layout::topo::layout::VisualGraph;

use std::collections::BTreeMap;

pub struct GraphBuilder<K: KontinuationAddress, V: ValueAddress> {
    graph: VisualGraph,
    style: StyleAttr,
    size: Point,
    lookup: BTreeMap<ProcState<K, V>, NodeHandle>,
}

impl<K: KontinuationAddress, V: ValueAddress> GraphBuilder<K, V> {
    pub fn new() -> Self {
        let graph = VisualGraph::new(Orientation::LeftToRight);
        let style = StyleAttr::simple();
        let size = Point::new(500., 500.);

        GraphBuilder {
            graph,
            style,
            size,

            lookup: BTreeMap::new(),
        }
    }

    pub fn add_node(&mut self, node: ProcState<K, V>) {
        let shape = ShapeKind::new_box(&format!(
            "{}\n{}\n{}\n{}\n{}",
            node.pid, node.prog_loc_or_pid, node.env, node.k_addr, node.time,
        ));

        let handle = self.graph.add_node(Element::create(
            shape,
            self.style.clone(),
            Orientation::LeftToRight,
            self.size,
        ));

        self.lookup.insert(node, handle);
    }

    pub fn add_edge(&mut self, start: ProcState<K, V>, end: ProcState<K, V>) {
        let arrow = Arrow::simple("");
        //if let Some(start_handle) = self.lookup.get(&start) {
        //    if let Some(end_handle) = self.lookup.get(&end) {
        //        self.graph.add_edge(arrow, *start_handle, *end_handle);
        //    }
        //}
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
