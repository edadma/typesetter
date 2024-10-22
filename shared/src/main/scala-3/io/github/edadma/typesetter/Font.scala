package io.github.edadma.typesetter

class Font(
    val typeface: String,
    val size: Double,
    //                 extents: FontExtents,
    val space: Double,
    val style: Set[String],
    val renderFont: Any,
    val baseline: Option[Double],
    val ligatures: Set[String],
)
