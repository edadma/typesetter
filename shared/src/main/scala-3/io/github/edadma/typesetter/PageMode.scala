package io.github.edadma.typesetter

class PageMode(t: Typesetter) extends VBoxBuilder(t):

  override infix def add(box: Box): Unit =
    val len = length

    super.add(box)

    if size > t.getNumber("vsize") then
      trimEnd(length - len)
      t.document add result
      clear()
      super.add(box)
    end if

  override def result: Box = wrap(buildTo(t.getNumber("vsize")))
