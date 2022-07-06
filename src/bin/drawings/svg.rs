use crate::eval::{Picture, Shape};
use std::io::{self, Write};
use xml::{
    writer::{self, XmlEvent},
    EmitterConfig, EventWriter,
};

impl Picture {
    /// Output an SVG representation of this picture to the writer `writer`.
    ///
    /// # Errors
    ///
    /// Returns `Err` if writing to `writer` produces an IO error.
    pub(crate) fn write_svg<W: Write>(&self, writer: W) -> io::Result<()> {
        let mut xml_writer = EmitterConfig::new()
            .perform_indent(true)
            .create_writer(writer);
        self.fmt_svg(&mut xml_writer).map_err(|e| match e {
            writer::Error::Io(e) => e,
            _ => unreachable!(),
        })
    }

    fn fmt_svg<W: Write>(&self, xml_writer: &mut EventWriter<W>) -> writer::Result<()> {
        let bbox = self.bounding_box();
        let viewbox = format!(
            "{} {} {} {}",
            bbox.lower_left.0,
            -bbox.upper_right.1,
            bbox.upper_right.0 - bbox.lower_left.0,
            -bbox.lower_left.1 + bbox.upper_right.1
        );
        xml_writer.write(
            XmlEvent::start_element("svg")
                .default_ns("http://www.w3.org/2000/svg")
                .attr("viewBox", &viewbox),
        )?;
        xml_writer.write(XmlEvent::start_element("style"))?;
        xml_writer.write(XmlEvent::cdata(
            r#"
            svg {
              width: 100vmin;
              height: 85vmin;
              margin: 0.5em auto;
              padding: 0.5em;
            }

            circle, line {
              fill: none;
              stroke: black;
              vector-effect: non-scaling-stroke;
            }
           "#,
        ))?;
        xml_writer.write(XmlEvent::end_element())?;

        for shape in &self.shapes {
            shape.fmt_svg(xml_writer)?;
        }

        xml_writer.write(XmlEvent::end_element())
    }
}

impl Shape {
    fn fmt_svg<W: Write>(&self, xml_writer: &mut EventWriter<W>) -> xml::writer::Result<()> {
        match self {
            Self::Circle {
                center: (cx, cy),
                radius: r,
            } => {
                let svg_y = -cy;
                xml_writer.write(
                    XmlEvent::start_element("circle")
                        .attr("cx", &cx.to_string())
                        .attr("cy", &svg_y.to_string())
                        .attr("r", &r.to_string()),
                )?;
                xml_writer.write(XmlEvent::end_element())
            }
            Self::Line {
                start: (x1, y1),
                end: (x2, y2),
            } => {
                let svg_y1 = -y1;
                let svg_y2 = -y2;
                xml_writer.write(
                    XmlEvent::start_element("line")
                        .attr("x1", &x1.to_string())
                        .attr("y1", &svg_y1.to_string())
                        .attr("x2", &x2.to_string())
                        .attr("y2", &svg_y2.to_string()),
                )?;
                xml_writer.write(XmlEvent::end_element())
            }
        }
    }
}
