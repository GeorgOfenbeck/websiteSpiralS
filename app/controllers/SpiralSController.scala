package controllers
import javax.inject.Inject
import models.SpiralS

import play.api.data._
import play.api.i18n._
import play.api.mvc._

/**
  * Created by rayda on 07-Jun-17.
  */
class SpiralSController @Inject()(val messagesApi: MessagesApi) extends Controller with I18nSupport {
  import SpiralSForm._

  private var spiralssettings: SpiralS =  SpiralS(2,2,false,true,false,false,false,"public/images/Graph.png", "")

  private val postUrl = routes.SpiralSController.generate()

  private val imgpath = "assets/images/Graph.png"


  def getImage(): String = "assets/images/Graph.png"

  def index = Action {
    Ok(views.html.index())
  }

  def listSpiralS = Action { implicit request: Request[AnyContent] =>
    // Pass an unpopulated form to the template
    Ok(views.html.listSpiralS(spiralssettings, form, postUrl))
  }

  // This will be the action that handles our form post
  def generate = Action { implicit request: Request[AnyContent] =>
    val errorFunction = { formWithErrors: Form[Data] =>
      assert(false)
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.listSpiralS(spiralssettings, formWithErrors, postUrl))
    }



    val successFunction = { data: Data =>
      // This is the good case, where the form was successfully parsed as a Data.
      val size = data.size
      val dyn = data.dyn
      val bsize = data.basesize
      val interleaved = data.interleaved
      val opsize: Option[Int] = if (dyn) None else Some(size)
      import SpiralSThesisGui.GuiThesis._
      val gen = new SpiralSThesis.CorewGlue(2,
          defradix.toMap.withDefaultValue(defradix(3)), opsize,
        interleaved, data.thread, data.basesize, data.twid_inline, data.twid_precomp
      )
      println(data)
      val codedump = gen.codeexport2()
      gen.graphexport(path = "C:\\Phd\\git\\code\\websiteSpiralS\\public\\images\\")


      spiralssettings = SpiralS(size = data.size, basesize = data.basesize, dyn = data.dyn, interleaved = data.interleaved,
        thread = data.thread,
        twidinline = data.twid_inline,
        twid_precomp =  data.twid_precomp,
        "assets/images/Graph.png", codedump)
      Redirect(routes.SpiralSController.listSpiralS()).flashing("info" -> "stuff")
    }

    val formValidationResult = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }




}