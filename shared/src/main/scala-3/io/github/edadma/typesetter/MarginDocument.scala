package io.github.edadma.typesetter

class MarginDocument(top: Double, right: Double, bottom: Double, left: Double) extends Document:
  def init(): Unit = ()

  def page(b: Box): Box = b
