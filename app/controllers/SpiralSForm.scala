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
  case class Data(size: Int, dyn: Boolean, interleaved: Boolean, basesize: Int, thread: Boolean, twid_inline: Boolean, twid_precomp: Boolean)

  /**
    * The form definition for the "create a widget" form.
    * It specifies the form fields and their types,
    * as well as how to convert from a Data to form data and vice versa.
    */
  val form = Form(
    mapping(
      "Transform size" -> number(min = 2),
      "size unknown at code generation time" -> boolean,
      "Complex format: interleaved?" -> boolean,
      "Basecase size" -> number(min = 2),

      "threaded" -> boolean,
      "Inline Twiddles if possible" -> boolean,
      "Precompute Twiddles" -> boolean
    )(Data.apply)(Data.unapply)
  )
}

