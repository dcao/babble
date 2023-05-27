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
            Self::Line { start, end } => {
                xml_writer.write(
                    XmlEvent::start_element("line")
                        .attr("x1", &start.x.to_string())
                        .attr("y1", &start.y.to_string())
                        .attr("x2", &end.x.to_string())
                        .attr("y2", &end.y.to_string()),
                )?;
                xml_writer.write(XmlEvent::end_element())
            }
            Self::Ellipse { transform, center } => {
                xml_writer.write(XmlEvent::start_element("circle").attr("r", "1").attr(
                    "transform",
                    &format!(
                        "matrix({} {} {} {} {} {})",
                        transform[(0, 0)],
                        transform[(0, 1)],
                        transform[(1, 0)],
                        transform[(1, 1)],
                        center.x,
                        center.y
                    ),
                ))?;
                xml_writer.write(XmlEvent::end_element())
                // let center = (focus_1 + focus_2) / 2;
                // let semi_major_axis = |vertex - center|;
                // let rotation = f64::acos((vertex - center).0/semi_major_axis)
            }
        }
    }
}
