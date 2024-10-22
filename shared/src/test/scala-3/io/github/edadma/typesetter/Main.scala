package io.github.edadma.typesetter

import scala.swing.*
import pprint.pprintln

import java.awt.image.BufferedImage

object Main extends SimpleSwingApplication:
  def top: Frame = new MainFrame:
    title = "Simple Swing Example"

    contents = new Panel {
      preferredSize = new Dimension(800, 500)

      override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)

        val doc = new SimpleDocument(5)
        val t =
          new Graphics2DTypesetter(doc):
            set("hsize", 400)
//            debug = true

//        t.hbox(t.getNumber("hsize"))
//          .addFil()
//          .add("Hello")
//          .add(" ")
//          .add("Scriptura!")
//          .add(" ")
//          .add("Cool")
//          .addFil()
//          .done()

//        t.hbox(t.getNumber("hsize"))
//          .addFil()
//          .add(
//            ImageBox(
//              t,
//              "866-536x354.jpg",
//            ),
//          )
//          .addFil()
//          .done()

//        t.hbox(t.getNumber("hsize"))
//          .addFil()
//          .add("one")
//          .addFil()
//          .done()
//
//        t.hbox(t.getNumber("hsize"))
//          .addFil()
//          .add("two")
//          .addFil()
//          .done()

//        t.hbox(t.getNumber("hsize"))
//          .addFil()
//          .add("three")
//          .addFil()
//          .done()

        t add "[qsdf"
        t.paragraph()
        t add "[qhTwer"
        t.paragraph()
        t.add(FillGlue)
        t.end()
        g.drawImage(doc.pages.head.asInstanceOf[BufferedImage], null, 0, 0)
      }
    }

    override def closeOperation(): Unit = dispose()
