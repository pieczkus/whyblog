package v1.comment

import javax.inject.Inject

import pl.why.common.SuccessResult
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class CommentInput(referenceUuid: String, referenceType: String, authorName: String, email: String, content: String)

class CommentController @Inject()(cc: ControllerComponents, handler: CommentResourceHandler)(implicit ec: ExecutionContext)
  extends AbstractController(cc) with I18nSupport {

  private lazy val form: Form[CommentInput] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "referenceUuid" -> nonEmptyText,
        "referenceType" -> nonEmptyText,
        "authorName" -> nonEmptyText,
        "email" -> nonEmptyText,
        "content" -> nonEmptyText
      )(CommentInput.apply)(CommentInput.unapply)
    )
  }

  def create: Action[AnyContent] = Action.async { implicit request =>
    processJsonCreateComment()
  }

  def find(referenceUuid: String): Action[AnyContent] = Action.async {
    handler.find(referenceUuid).map { comments =>
      Ok(Json.toJson(comments))
    }
  }

  private def processJsonCreateComment[A]()(
    implicit request: Request[A]): Future[Result] = {
    def failure(badForm: Form[CommentInput]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(input: CommentInput) = {
      handler.create(input).map {
        case SuccessResult => Created
        case _ => BadRequest
      }
    }

    form.bindFromRequest().fold(failure, success)
  }

}
