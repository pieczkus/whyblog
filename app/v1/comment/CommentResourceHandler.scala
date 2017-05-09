package v1.comment

import javax.inject.{Inject, Named, Provider}

import akka.actor.ActorRef
import akka.util.Timeout
import pl.why.common.{EmptyResult, FullResult, ServiceResult, SuccessResult}
import play.api.libs.json.{JsValue, Json, Writes}
import v1.comment.command.CommentData
import v1.comment.command.CommentManager.AddComment
import v1.comment.query.CommentView.FindComments
import v1.comment.query.CommentViewBuilder.CommentRM

import scala.concurrent.{ExecutionContext, Future}

case class CommentResource(uuid: String, referenceUuid: String, authorName: String, content: String, createdOn: Long, published: Boolean)

object CommentResource {

  implicit val implicitWrites = new Writes[CommentResource] {
    def writes(c: CommentResource): JsValue = {
      Json.obj(
        "uuid" -> c.uuid,
        "referenceUuid" -> c.referenceUuid,
        "authorName" -> c.authorName,
        "content" -> c.content,
        "createdOn" -> c.createdOn,
        "published" -> c.published
      )
    }
  }

}

class CommentResourceHandler @Inject()(routerProvider: Provider[CommentRouter],
                                       @Named("comment-manager") commentManager: ActorRef, @Named("comment-view") commentView: ActorRef)
                                      (implicit ec: ExecutionContext) {

  import akka.pattern.ask
  import scala.concurrent.duration._

  implicit val timeout: Timeout = 5.seconds


  def find(referenceUuid: String): Future[Seq[CommentResource]] = {
    (commentView ? FindComments(referenceUuid)).mapTo[ServiceResult[Seq[CommentRM]]].map {
      case FullResult(comments) => comments.map(createCommentResource)
      case _ => Seq.empty
    }
  }

  def create(input: CommentInput): Future[ServiceResult[Any]] = {
    (commentManager ? AddComment(input)).mapTo[ServiceResult[CommentData]].map {
      case FullResult(_) => SuccessResult
      case _ => EmptyResult
    }
  }

  private def createCommentResource(c: CommentRM): CommentResource = {
    CommentResource(c.uuid, c.referenceUuid, c.authorName, c.content, c.createdOn, c.published)
  }

}

