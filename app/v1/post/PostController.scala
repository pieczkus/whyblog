package v1.post

import javax.inject.Inject

import auth.AuthorizedAction
import pl.why.common.SuccessResult
import play.api.data.Form
import play.api.i18n.I18nSupport
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

case class BodyComponentInput(component: String, parameters: List[(String, String)])

case class AddPostInput(author: String, title: String, body: List[BodyComponentInput], coverUrl: String, tags: List[String],
                        metaTitle: String, metaDescription: String, metaKeywords: String)

class PostController @Inject()(cc: ControllerComponents, handler: PostResourceHandler, authorizedAction: AuthorizedAction)(implicit ec: ExecutionContext)
  extends AbstractController(cc) with I18nSupport {

  private final val API_KEY_HEADER = "Why-Key"

  private lazy val form: Form[AddPostInput] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "author" -> nonEmptyText,
        "title" -> nonEmptyText,
        "body" -> list(
          mapping(
            "component" -> nonEmptyText,
            "parameters" -> list(tuple(
              "name" -> nonEmptyText,
              "value" -> nonEmptyText
            ))
          )(BodyComponentInput.apply)(BodyComponentInput.unapply)),
        "coverUrl" -> nonEmptyText,
        "tags" -> list(nonEmptyText),
        "metaTitle" -> nonEmptyText,
        "metaDescription" -> nonEmptyText,
        "metaKeywords" -> nonEmptyText
      )(AddPostInput.apply)(AddPostInput.unapply)
    )
  }

  def create: Action[AnyContent] = authorizedAction.async { implicit request =>
    processJsonCreatePost()
  }

  def findByTitle(title: String): Action[AnyContent] = Action.async { implicit request =>
    if (request.headers.get(API_KEY_HEADER).isEmpty) {
      Future.failed(new IllegalArgumentException("missing why-key header"))
    } else {
      handler.findByTitle(request.headers(API_KEY_HEADER), title).map {
        case Some(p) => Ok(Json.toJson(p))
        case _ => NotFound
      }
    }
  }

  def publish(title: String): Action[AnyContent] = Action.async { implicit request =>
    if (request.headers.get(API_KEY_HEADER).isEmpty) {
      Future.failed(new IllegalArgumentException("missing why-key header"))
    } else {
      handler.publish(request.headers(API_KEY_HEADER), title).map {
        case SuccessResult => Ok
        case _ => BadRequest
      }
    }
  }

  def find: Action[AnyContent] = Action.async { implicit request =>
    if (request.headers.get(API_KEY_HEADER).isEmpty) {
      Future.failed(new IllegalArgumentException("missing why-key header"))
    } else {
      handler.find(request.headers(API_KEY_HEADER), None).map { posts =>
        Ok(Json.toJson(posts))
      }
    }
  }

  def findPinnedPost: Action[AnyContent] = Action.async { implicit request =>
    if (request.headers.get(API_KEY_HEADER).isEmpty) {
      Future.failed(new IllegalArgumentException("missing why-key header"))
    } else {
      handler.findPinnedPost(request.headers(API_KEY_HEADER)).map {
        case Some(p) => Ok(Json.toJson(p))
        case _ => NotFound
      }
    }
  }

  def pinPost(title: String): Action[AnyContent] = Action.async { implicit request =>
    if (request.headers.get(API_KEY_HEADER).isEmpty) {
      Future.failed(new IllegalArgumentException("missing why-key header"))
    } else {
      handler.pinPost(request.headers(API_KEY_HEADER), title).map {
        case SuccessResult => Ok
        case _ => BadRequest
      }
    }
  }

  def findNotPublished = authorizedAction.async { implicit request =>
    handler.findNotPublished(request.headers(API_KEY_HEADER), None).map { posts =>
      Ok(Json.toJson(posts))
    }
  }

  def findByTag(tag: String): Action[AnyContent] = Action.async {
    implicit request =>
      if (request.headers.get(API_KEY_HEADER).isEmpty) {
        Future.failed(new IllegalArgumentException("missing why-key header"))
      } else {
        handler.findByTag(request.headers(API_KEY_HEADER), tag).map { posts =>
          Ok(Json.toJson(posts))
        }
      }
  }

  private def processJsonCreatePost[A]()(
    implicit request: Request[A]): Future[Result] = {
    def failure(badForm: Form[AddPostInput]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(input: AddPostInput) = {
      handler.create(request.headers(API_KEY_HEADER), input).map {
        case SuccessResult => Created
        case _ => BadRequest
      }
    }

    form.bindFromRequest().fold(failure, success)
  }

}
