package io.github.edadma

import java.awt.GraphicsEnvironment


object FontsMain extends App
{
	println( GraphicsEnvironment.getLocalGraphicsEnvironment.getAllFonts filter (_.getFamily matches "Free.*") mkString (", ") )
}