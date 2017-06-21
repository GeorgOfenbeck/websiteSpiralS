package models

/**
  * Presentation object used for displaying data in a template.
  *
  * Note that it's a good practice to keep the presentation DTO,
  * which are used for reads, distinct from the form processing DTO,
  * which are used for writes.
  */
case class SpiralS(size: Int, dyn: Boolean, interleaved: Boolean, basesize: Int, thread: Boolean, twidinline: Boolean, twid_precomp: Boolean, img: String, txt: String)

