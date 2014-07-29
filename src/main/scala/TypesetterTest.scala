package typesetter

import scala.swing._
import java.awt.RenderingHints._
import java.awt.Color._
import java.awt.Font._
import java.awt.geom._
import java.io._


object TypesetterTest extends MainFrame with App
{
	val MARGIN = 5
	val t = new Typesetter
	
	val fs = System.getProperties.getProperty( "file.separator" )
	val home = System.getProperties.getProperty( "user.home" )
	val fonts = home + fs + "Dropbox" + fs + "Typography" + fs + "Fonts" + fs
	
//	println( t.dimen("2parindent") )
//	t.font( Font(null, fonts + "GentiumPlus-1.510" + fs + "GentiumPlus-R.ttf", "plain", 30) )

//	t.vbox
//	t.hbox
//	t text "line 1"
//	t.end
//	t.vskip( 5, 0, 0 )
//	t.hrule( 1 )
//	t.vskip( 5, 0, 0 )
//	t.hbox
//	t text "line 2"
//	t.end
	
	t.vertical
//	t text "framed"
//	t.space
//	t.frame( "text items", 3, 1 )
//	t.space
//	t shift ("is", -5)
//	t.hrule( 10, 0, 5 )
//	t.space
//	t.list
//	t text "/"
//	t.rlap( t.arg )
//	t text "="
//	t.space
//	t.underline( "cool" )
//	t.par
//
//	t text "moved"
//	t.space
//	t.vbox
//	t.hbox( To(100) )
//	t text "text 1"
//	t.end
//	t.hbox
//	t text "text 2"
//	t.move( t.box, 30 )
//	t.end
//	t.space
//	t text "wow"
//	t.par
//	
//	t text "rounded"
//	t.space
//	t add new ShapeBox( new RoundRectangle2D.Double(0, 0, 50, 25, 20, 20), false, t.color, t.alpha, "Box", 0 )
//	t.space
//	t text "rectangle"
//	t.par
//	
//	t vskip 5
//	t hrule 1
//	t vskip 5

//	t text "This is a very very very very very very very very very very very very very very very very very very very boring test."
//	t.par
//	
//	t.vbox
//	t.variable( 'hsize, 600 )
//	t.hbox
//	t text "top line"
//	t.end
//	t text "This is a very very very very very very very very very very very very very very very very very very very boring test."
//	t.par
//	t.hbox
//	t text "bottom line"
//	t.end
//	t.end
//
//	t text "This is a very very very very very very very very very very very very very very very very very very very boring test."
//	t.par
	
//	t.list
//	t text "1."
//	t.item( t.arg )
//	t text "first item ljksdfljk fdsjkl fd lkjfdsjkl fd jlkfsdl jkfdsl jkfa ljkfd ljkfds ljkfds jlkf jlkfds jfds"
//	
//	t.list
//	t text "a)"
//	t.itemitem( t.arg )
//	t text "first sub-item ljksdfljk fdsjkl fd lkjfdsjkl fd jlkfsdl jkfdsl jkfa ljkfd ljkfds ljkfds jlkf jlkfds jfds"
//	
//	t.list
//	t text "b)"
//	t.itemitem( t.arg )
//	t text "second sub-item ljksdfljk fdsjkl fd lkjfdsjkl fd jlkfsdl jkfdsl jkfa ljkfd ljkfds ljkfds jlkf jlkfds jfds"
//
//	t.list
//	t text "2."
//	t.item( t.arg )
//	t text "second item ljksdfljk fdsjkl fd lkjfdsjkl fd jlkfsdl jkfdsl jkfa ljkfd ljkfds ljkfds jlkf jlkfds jfds"
	
	t text """
			You don't know about me without you have read a book by the name of
			``The Adventures of Tom Sawyer;'' but that ain't no matter. That book
			was made by Mr. Mark Twain, and he told the truth, mainly. There was
			things which he stretched, but mainly he told the truth. That is
			nothing. I never seen anybody but lied one time or another, without it
			was Aunt Polly, or the widow, or maybe Mary. Aunt Polly---Tom's Aunt
			Polly, she is---and Mary, and the Widow Douglas is all told about in
			that book, which is mostly a true book, with some stretchers, as I
			said before.
		"""
	t.par
	
//	t.draw( None )
//	
//	val path = new Path2D.Double
//	
//		path moveTo (0, 0)
//		path lineTo (10, 10)
//		path lineTo (20, -10)
//		
//	t.string( "0" )
//	t.string( "0" )
//	t.string( "org" )
//	t += new ShapeBox( path, false, t.color, t.alpha, t.stroke, "", 0 )
//	t.end
//	
//	t.rectangle( 50, 50, false )
//	t.text( "f ff \ufb00" )
	
	val p = t box

	contents =
		new Panel
		{
			background = WHITE
			preferredSize = new Dimension( 800, 600 )
			
			override def paint( g: Graphics2D )
			{
				println( g.getDeviceConfiguration.getNormalizingTransform )
				super.paint( g )
				g.setRenderingHint( KEY_ANTIALIASING, VALUE_ANTIALIAS_ON )
				p.draw( g, MARGIN, MARGIN )
//				p.box( g, MARGIN, MARGIN, CYAN )
			}
		}
	visible = true
}
