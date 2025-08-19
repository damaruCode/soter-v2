use crate::state_space::KontinuationAddress;
use crate::state_space::ProcState;
use crate::state_space::ValueAddress;
use crate::util::SetMap;

use layout::adt::dag::NodeHandle;
use layout::backends::svg::SVGWriter;
use layout::core::base::Orientation;
use layout::core::geometry::Point;
use layout::core::style::*;
use layout::core::utils::save_to_file;
use layout::std_shapes::shapes::*;
use layout::topo::layout::VisualGraph;

pub struct GraphBuilder<K: KontinuationAddress, V: ValueAddress> {
    graph: VisualGraph,
    style: StyleAttr,
    size: Point,
    lookup: Vec<(ProcState<K, V>, NodeHandle)>,
}

impl<K: KontinuationAddress, V: ValueAddress> GraphBuilder<K, V> {
    pub fn new() -> Self {
        let mut graph = VisualGraph::new(Orientation::LeftToRight);
        let shape = ShapeKind::new_circle("Main");
        let style = StyleAttr::simple();
        let size = Point::new(100., 100.);

        let handle = graph.add_node(Element::create(
            shape,
            style.clone(),
            Orientation::LeftToRight,
            size,
        ));

        let mut lookup = vec![(0, handle)];

        GraphBuilder {
            graph,
            style,
            size,

            lookup,
        }
    }

    pub fn add_node(&mut self, node: ProcState<K, V>) {
        let shape = ShapeKind::new_circle("todo");

        let handle = self.graph.add_node(Element::create(
            shape,
            self.style.clone(),
            Orientation::LeftToRight,
            self.size,
        ));

        self.lookup.push((node, handle));
    }

    pub fn add_edge(&mut self, start: &ProcState<K, V>, end: &ProcState<K, V>) {
        let arrow = Arrow::simple("todo");

        for 

        self.graph.add_edge(
            arrow,
            *self.lookup.get(start).unwrap(),
            *self.lookup.get(end).unwrap(),
        );
    }

    pub fn print(&mut self) {
        // Render the nodes to some rendering backend.
        let mut svg = SVGWriter::new();
        self.graph.do_it(false, false, false, &mut svg);

        // Save the output.
        let _ = save_to_file("./graphs/graph.svg", &svg.finalize());
    }
}
