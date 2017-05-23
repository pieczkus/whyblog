package v1.post

import javax.inject.{Inject, Named, Provider}

import akka.actor.ActorRef
import akka.util.Timeout
import pl.why.common.PersistentEntity.GetState
import pl.why.common.lookup.SimpleResponse
import pl.why.common.{EmptyResult, FullResult, ServiceResult, SuccessResult}
import play.api.libs.json.{JsValue, Json, Writes}
import v1.post.command.{BodyComponentData, PostData}
import v1.post.command.PostManager._
import v1.post.read.PageRequest
import v1.post.read.PostView._
import v1.post.read.PostViewBuilder.PostRM

import scala.concurrent.{ExecutionContext, Future}

case class PostResource(postId: String, author: String, title: String, body: Seq[BodyComponentData], coverUrl: String,
                        metaTitle: String, metaDescription: String, metaKeywords: String, publishedOn: Long,
                        commentCount: Int, timeToRead: String, tags: List[String], relatedPosts: Seq[String],
                        pinned: Boolean)

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
        "postId" -> p.postId,
        "author" -> p.author,
        "title" -> p.title,
        "body" -> p.body,
        "coverUrl" -> p.coverUrl,
        "metaTitle" -> p.metaTitle,
        "metaDescription" -> p.metaDescription,
        "metaKeywords" -> p.metaKeywords,
        "publishedOn" -> p.publishedOn,
        "commentCount" -> p.commentCount,
        "timeToRead" -> p.timeToRead,
        "tags" -> p.tags,
        "relatedPosts" -> p.relatedPosts,
        "pinned" -> p.pinned
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

  def create(key: String, input: AddPostInput): Future[ServiceResult[Any]] = {
    (postManager ? AddPost(key, input)).mapTo[ServiceResult[PostData]].map {
      case FullResult(_) => SuccessResult
      case _ => EmptyResult
    }
  }

  def findByTitle(key: String, title: String): Future[Option[PostResource]] = {
    (postView ? FindPostByTitle(key, title)).mapTo[ServiceResult[Seq[PostRM]]].map {
      case FullResult(posts) if posts.nonEmpty => Some(createCommentResource(posts.head))
      case _ => None
    }
  }

  def publish(key: String, title: String): Future[ServiceResult[Any]] = {
    (postManager ? AnnouncePost(key, title)).mapTo[ServiceResult[PostData]].map {
      case FullResult(_) => SuccessResult
      case _ => EmptyResult
    }
  }

  def find(key: String, page: Option[PageRequest]): Future[Seq[PostResource]] = {
    (postView ? FindPublishedPosts(key, page)).mapTo[ServiceResult[Seq[PostRM]]].map {
      case FullResult(posts) => posts.map(createCommentResource)
      case _ => Seq.empty
    }
  }

  def findPinnedPost(key: String): Future[Option[PostResource]] = {
    (postView ? FindPinnedPost(key)).mapTo[ServiceResult[Seq[PostRM]]].map {
      case FullResult(posts) if posts.nonEmpty => Some(createCommentResource(posts.head))
      case _ => None
    }
  }

  def pinPost(key: String, title: String): Future[ServiceResult[Any]] = {
    (postView ? FindPinnedPost(key)).mapTo[ServiceResult[Seq[PostRM]]].map {
      case FullResult(posts) if posts.nonEmpty =>
        posts.foreach(p => {
          postManager ! UnpinPost(key, p.title)
        })
        postManager ! PinPost(key, title)
        SuccessResult
      case _ =>
        postManager ! PinPost(key, title)
        SuccessResult
    }
  }

  def findNotPublished(key: String, page: Option[PageRequest]): Future[Seq[PostResource]] = {
    (postView ? FindNotPublishedPosts(key, page)).mapTo[ServiceResult[Seq[PostRM]]].map {
      case FullResult(posts) => posts.map(createCommentResource)
      case _ => Seq.empty
    }
  }

  def findByTag(key: String, tag: String): Future[Seq[PostResource]] = {
    (postView ? FindPostsByTag(key, tag)).mapTo[ServiceResult[Seq[PostRM]]].map {
      case FullResult(posts) => posts.map(createCommentResource)
      case _ => Seq.empty
    }
  }

  def findAfter(key: String, after: Long): Future[Option[PostResource]] = {
    (postView ? FindPostPublishedAfter(key, after)).mapTo[ServiceResult[Seq[PostRM]]].map {
      case FullResult(posts) if posts.nonEmpty => Some(createCommentResource(posts.head))
      case _ => None
    }
  }

  def findBefore(key: String, before: Long): Future[Option[PostResource]] = {
    (postView ? FindPostPublishedBefore(key, before)).mapTo[ServiceResult[Seq[PostRM]]].map {
      case FullResult(posts) if posts.nonEmpty => Some(createCommentResource(posts.head))
      case _ => None
    }
  }

  def find(postId: String): Future[Option[PostResource]] = {
    (postManager ? FindPostById(postId)).mapTo[ServiceResult[PostData]].map {
      case FullResult(post) => Some(createCommentResource(post))
      case _ => None
    }
  }

  def addRelatedPost(postId: String, relatedPostId: String): Future[ServiceResult[Any]] = {
    (postManager ? AddRelated(postId, relatedPostId)).mapTo[ServiceResult[PostData]].map {
      case FullResult(_) => SuccessResult
      case _ => EmptyResult
    }
  }

  def incrementCommentCount(postId: String): Future[ServiceResult[Any]] = {
    (postManager ? AddComment(postId)).mapTo[ServiceResult[PostData]].map {
      case FullResult(_) => SuccessResult
      case _ => EmptyResult
    }
  }

  private def createCommentResource(p: PostRM): PostResource = {
    PostResource(p.postId, p.author, p.title, p.body, p.coverUrl, p.metaTitle, p.metaDescription, p.metaKeywords, p.publishedOn,
      p.commentCount, p.timeToRead, p.tags, p.relatedPosts, p.pinned)
  }

  private def createCommentResource(p: PostData): PostResource = {
    PostResource(p.postId, p.author, p.title, p.body, p.coverUrl, p.metaTitle, p.metaDescription, p.metaKeywords, p.publishedOn,
      p.commentCount, p.timeToRead, p.tags, p.relatedPosts, p.pinned)
  }

}
