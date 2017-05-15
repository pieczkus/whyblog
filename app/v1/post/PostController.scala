package v1.post

import javax.inject.Inject

import auth.AuthorizedAction
import pl.why.common.SuccessResult
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class BodyComponentInput(component: String, parameters: List[String])

case class AddPostInput(key: String, author: String, title: String, body: List[(String, List[(String, String)])], coverUrl: String, tags: List[String],
                        metaTitle: String, metaDescription: String, metaKeywords: String)

class PostController @Inject()(cc: ControllerComponents, handler: PostResourceHandler, authorizedAction: AuthorizedAction)(implicit ec: ExecutionContext)
  extends AbstractController(cc) with I18nSupport {

  private lazy val form: Form[AddPostInput] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "key" -> nonEmptyText,
        "author" -> nonEmptyText,
        "title" -> nonEmptyText,
        "body" -> list(tuple(
          "component" -> nonEmptyText,
          "parameters" -> list(tuple(
            "key" -> nonEmptyText,
            "value" -> nonEmptyText
          ))
        )),
        "coverUrl" -> nonEmptyText,
        "tags" -> list(nonEmptyText),
        "metaTitle" -> nonEmptyText,
        "metaDescription" -> nonEmptyText,
        "metaKeywords" -> nonEmptyText
      )(AddPostInput.apply)(AddPostInput.unapply)
    )
  }

  def create = authorizedAction.async { implicit request =>
    processJsonCreatePost()
  }

  def findByTitle(title: String): Action[AnyContent] = Action.async { implicit request =>
    if (request.headers.get("why-key").isEmpty) {
      Future.failed(new IllegalArgumentException("missing why-key header"))
    } else {
      handler.findByTitle(request.headers("why-key"), title).map {
        case Some(p) => Ok(Json.toJson(p))
        case _ => BadRequest
      }
    }
  }

  private def processJsonCreatePost[A]()(
    implicit request: Request[A]): Future[Result] = {
    def failure(badForm: Form[AddPostInput]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(input: AddPostInput) = {
      handler.create(input).map {
        case SuccessResult => Created
        case _ => BadRequest
      }
    }

    form.bindFromRequest().fold(failure, success)
  }

}
