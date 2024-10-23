package io.github.edadma.typesetter

class MarginDocument(top: Double, right: Double, bottom: Double, left: Double) extends Document:
  def init(): Unit = ()

  def layout(b: Box): Box = b

  infix def add(box: Box): Unit = ()
