package controllers

object SpiralSForm {
  import play.api.data.Forms._
  import play.api.data.Form

  /**
    * A form processing DTO that maps to the form below.
    *
    * Using a class specifically for form binding reduces the chances
    * of a parameter tampering attack and makes code clearer.
    */
  case class Data(size: Int,  basesize: Int, dyn: Boolean, interleaved: Boolean, thread: Boolean, twid_inline: Boolean, twid_precomp: Boolean)

  /**
    * The form definition for the "create a widget" form.
    * It specifies the form fields and their types,
    * as well as how to convert from a Data to form data and vice versa.
    */
  val form = Form(
    mapping(
      "size" -> number(min = 2),
      "basesize" -> number(min = 2),
      "dyn" -> boolean,
      "interleaved" -> boolean,
      "thread" -> boolean,
      "twid_inline" -> boolean,
      "twid_precomp" -> boolean
    )(Data.apply)(Data.unapply)
  )

    /*mapping(
      "Transform size" -> number(min = 2),
      "Basecase size" -> number(min = 2),
      "size unknown at code generation time" -> boolean,
      "Complex format: interleaved?" -> boolean,
      "threaded" -> boolean,
      "Inline Twiddles if possible" -> boolean,
      "Precompute Twiddles" -> boolean
    )(Data.apply)(Data.unapply)
  )*/
}

