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

  private var spiralssettings: SpiralS = SpiralS(0,"public/images/Graph.png")

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
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.listSpiralS(spiralssettings, formWithErrors, postUrl))
    }



    val successFunction = { data: Data =>
      // This is the good case, where the form was successfully parsed as a Data.
      val size = data.size
      import SpiralSThesisGui.GuiThesis._
      val gen = new SpiralSThesis.CorewGlue(size,
          defradix.toMap.withDefaultValue(defradix(3)))

      gen.codeexport()
      gen.graphexport(path = "F:\\Phd\\git\\code\\play-scala-forms-example\\public\\images\\")

      spiralssettings = SpiralS(size = data.size,"assets/images/Graph.png")
      Redirect(routes.SpiralSController.listSpiralS()).flashing("info" -> "Widget added!")
    }

    val formValidationResult = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }




}