package io.github.edadma.typesetter

class PageMode(t: Typesetter) extends VBoxBuilder(t):

  override infix def add(box: Box): Unit =
    val len = length

    super.add(box)

    if size > t.getNumber("vsize") then
      trimEnd(length - len)
      newpage()
      super.add(box)
    end if

  def newpage(): Unit =
    t.document add result
    clear()

  override def result: Box = wrap(buildTo(t.getNumber("vsize")))
