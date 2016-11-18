package xyz.hyperreal.typesetter

import java.awt.GraphicsEnvironment


object FontsMain extends App
{
	println( GraphicsEnvironment.getLocalGraphicsEnvironment.getAllFonts filter (_.getFamily matches "Free.*") mkString (", ") )
}