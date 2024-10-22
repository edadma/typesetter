package io.github.edadma.typesetter

import scala.language.postfixOps

object Hyphenation:
  val words: Map[String, Seq[String]] =
    List(
      "al-go-rithm",
      "pres-by-te-ri-an",
      "Em-bry-ol-o-gy",
      "car-diato-my",
      "bud-get",
      "wilder-ness",
      "lam-en-ta-ble",
      "there-in",
      "light-ed",
      "stand-ing",
      "trem-bled",
      "dream-ed",
      "gen-tle-ness",
      "faith-ful-ness",
      "dis-tem-per",
      "over-throw",
      "some-times",
    ) map (w => w.filterNot(_ == '-') -> w.split('-').toSeq) toMap

  def apply(word: String): Option[Iterator[(String, String)]] =
    words get word map { s =>
      new Iterator[(String, String)]:
        var idx = 1

        override def hasNext: Boolean = idx < s.length

        override def next: (String, String) =
          val (before, after) = s.splitAt(idx)

          idx += 1
          (before.mkString :+ '-', after.mkString)
    }
