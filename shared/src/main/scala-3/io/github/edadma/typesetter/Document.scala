package io.github.edadma.typesetter

import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

abstract class Document extends Mode:
  private[typesetter] var ts: Typesetter = uninitialized
  val pages = new ArrayBuffer[Any]

  def t: Typesetter = ts

  def init(): Unit

  def page(b: Box): Box

  infix def add(box: Box): Unit =
    pages += t.render(
      page(box),
      t.getNumber("pagewidth"),
      t.getNumber("pageheight"),
      t.getNumber("hoffset"),
      t.getNumber("voffset"),
    )

  override def done(): Unit = pop

  def result: Box = ???
