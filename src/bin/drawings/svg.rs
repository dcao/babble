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
        xml_writer.write(
            XmlEvent::start_element("svg")
                .default_ns("http://www.w3.org/2000/svg")
                .attr("viewBox", "-25 -25 50 50"),
        )?;
        xml_writer.write(XmlEvent::start_element("style"))?;
        xml_writer.write(XmlEvent::cdata(
            r#"
            svg {
              width: 100vmin;
              height: 100vmin;
              margin: 0 auto;
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
                xml_writer.write(
                    XmlEvent::start_element("circle")
                        .attr("cx", &cx.to_string())
                        .attr("cy", &cy.to_string())
                        .attr("r", &r.to_string()),
                )?;
                xml_writer.write(XmlEvent::end_element())
            }
            Self::Line {
                start: (x1, y1),
                end: (x2, y2),
            } => {
                xml_writer.write(
                    XmlEvent::start_element("line")
                        .attr("x1", &x1.to_string())
                        .attr("y1", &y1.to_string())
                        .attr("x2", &x2.to_string())
                        .attr("y2", &y2.to_string()),
                )?;
                xml_writer.write(XmlEvent::end_element())
            }
        }
    }
}
