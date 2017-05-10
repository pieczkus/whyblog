package v1.post

import javax.inject.{Inject, Named, Provider}

import akka.actor.ActorRef
import akka.util.Timeout
import pl.why.common.{EmptyResult, FullResult, ServiceResult, SuccessResult}
import play.api.libs.json.{JsValue, Json, Writes}
import v1.post.command.{BodyComponentData, PostData}
import v1.post.command.PostManager.AddPost
import v1.post.read.PostView.FindPostByTitle
import v1.post.read.PostViewBuilder.PostRM

import scala.concurrent.{ExecutionContext, Future}

case class PostResource(author: String, title: String, body: Seq[BodyComponentData], coverUrl: String,
                        metaTitle: String, metaDescription: String, metaKeywords: String, publishedOn: Long,
                        commentCount: Int, timeToRead: String, tags: List[String], relatedPosts: Seq[String])

object PostResource {

  implicit val implicitBodyComponentWrites = new Writes[BodyComponentData] {
    def writes(b: BodyComponentData): JsValue = {
      Json.obj(
        "component" -> b.component,
        "parameters" -> Json.arr(b.parameters)
      )
    }
  }

  implicit val implicitPostResourceWrites = new Writes[PostResource] {
    def writes(p: PostResource): JsValue = {
      Json.obj(
        "authore" -> p.author,
        "title" -> p.title,
        "body" -> Json.arr(p.body),
        "coverUrl" -> p.coverUrl,
        "metaTitle" -> p.metaTitle,
        "metaDescription" -> p.metaDescription,
        "metaKeywords" -> p.metaKeywords,
        "publishedOn" -> p.publishedOn,
        "commentCount" -> p.commentCount,
        "timeToRead" -> p.timeToRead,
        "tags" -> Json.arr(p.tags),
        "relatedPosts" -> Json.arr(p.relatedPosts)
      )
    }
  }

}

class PostResourceHandler @Inject()(routerProvider: Provider[PostRouter],
                                    @Named("post-manager") postManager: ActorRef,
                                    @Named("post-view") postView: ActorRef)
                                   (implicit ec: ExecutionContext) {

  import akka.pattern.ask
  import scala.concurrent.duration._

  implicit val timeout: Timeout = 5.seconds

  def create(input: AddPostInput): Future[ServiceResult[Any]] = {
    (postManager ? AddPost(input)).mapTo[ServiceResult[PostData]].map {
      case FullResult(_) => SuccessResult
      case _ => EmptyResult
    }
  }

  def findByTitle(key: String, title: String): Future[Option[PostResource]] = {
    (postView ? FindPostByTitle(key, title)).mapTo[ServiceResult[Seq[PostRM]]].map {
      case FullResult(posts) => Some(createCommentResource(posts.head))
      case _ => None
    }
  }

  private def createCommentResource(p: PostRM): PostResource = {
    PostResource(p.author, p.title, p.body, p.coverUrl, p.metaTitle, p.metaDescription, p.metaKeywords, p.publishedOn, p.commentCount, p.timeToRead, p.tags, p.relatedPosts)
  }

}
