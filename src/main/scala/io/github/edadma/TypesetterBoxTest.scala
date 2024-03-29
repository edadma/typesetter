package io.github.edadma

import java.awt.Color.*
import java.awt.Font.*
import java.awt.RenderingHints.*
import java.awt.geom.*
import java.io.*
import scala.swing.{Font as SF, *}


object TypesetterBoxTest extends MainFrame with App
{
	val M = 50
	val t = new Typesetter
	
	val fs = System.getProperties.getProperty( "file.separator" )
	val home = System.getProperties.getProperty( "user.home" )
	val fonts = home + fs + "dev" + fs + "typesetter" + fs + "fonts" + fs

	t.font( Font(null, fonts + "GentiumPlus-R.ttf", "plain", 300) )
	t color GRAY
	t.list
	t.glyphs( "p", true )
	
	val p = t.arg.head.asInstanceOf[GlyphBox]
	val C = 200
	
	contents =
		new Panel
		{
			background = WHITE
			preferredSize = new Dimension( 800, 600 )
			
			override def paint( g: Graphics2D ) = {
				def cross( x: Double, y: Double ) = {
					g setColor RED
					g.draw( new Line2D.Double(x, y - C, x, y + C) )
					g.draw( new Line2D.Double(x - C, y, x + C, y) )
				}
				
				super.paint( g )
				g.setRenderingHint( KEY_ANTIALIASING, VALUE_ANTIALIAS_ON )
				p.draw( g, M, M )
				cross( M, M )
				cross( M, M + p.ascent )
				cross( M, M + p.height )
				cross( M + p.vbounds.getX, M )
				cross( M + p.lbounds.getWidth, M )// + p.lbounds.getHeight )
				cross( M + p.vbounds.getWidth, M + p.vbounds.getHeight )
				println( p.lbounds.getHeight, p.lbounds.getY )
//				p.box( g, MARGIN, MARGIN, BLUE )
			}
		}
	visible = true
}