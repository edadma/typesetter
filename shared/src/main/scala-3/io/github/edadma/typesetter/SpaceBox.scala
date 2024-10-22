package io.github.edadma.typesetter

trait SpaceBox extends Box:
  val ascent: Double = 0
  val isSpace: Boolean = true

  def draw(t: Typesetter, x: Double, y: Double): Unit =
    box(t, x, y, "lightgreen")
