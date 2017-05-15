package v1.post.command

import java.util.Date

import akka.actor.Props
import com.trueaccord.scalapb.GeneratedMessage
import pl.why.common._
import pl.why.blog.proto.Blog
import v1.post.command.PostEntity.Command.{AddRelatedPost, CreatePost, PublishPost}
import v1.post.command.PostEntity.Event.{PostCreated, PostPublished, RelatedPostAdded}

case class BodyComponentData(component: String, parameters: Map[String, String])

case class PostData(postId: String, key: String, author: String, title: String, body: Seq[BodyComponentData], coverUrl: String,
                    metaTitle: String, metaDescription: String, metaKeywords: String, publishedOn: Long = 0L,
                    commentCount: Int = 0, timeToRead: String = "", tags: List[String] = List.empty,
                    relatedPosts: Seq[String] = List.empty, deleted: Boolean = false) extends EntityFieldsObject[String, PostData] {
  def assignId(id: String): PostData = this.copy(postId = id)

  def id: String = postId

  def markDeleted: PostData = this.copy(deleted = true)
}

object PostData {
  val empty = PostData("", "", "", "", Seq.empty, "", "", "", "")
}

object PostEntity {
  val EntityType = "post"

  object Command {

    case class CreatePost(post: PostData) extends EntityCommand {
      def entityId: String = post.postId
    }

    case class PublishPost(id: String) extends EntityCommand {
      def entityId: String = id
    }

    case class AddRelatedPost(relatedPostId: String, id: String) extends EntityCommand {
      def entityId: String = id
    }

  }

  object Event {

    trait PostEvent extends EntityEvent {
      def entityType = EntityType
    }

    case class PostCreated(p: PostData) extends PostEvent {
      override def toDataModel: Blog.PostCreated = {

        val body = p.body.map(b => Blog.BodyComponent(b.component, b.parameters))

        val post = Blog.Post(p.postId, p.key, p.author, p.title, body, p.coverUrl, p.metaTitle, p.metaDescription, p.metaKeywords,
          p.publishedOn, p.commentCount, p.timeToRead, p.tags, p.relatedPosts, p.deleted)
        Blog.PostCreated(Some(post))
      }
    }

    object PostCreated extends DataModelReader {
      def fromDataModel: PartialFunction[GeneratedMessage, PostCreated] = {
        case dm: Blog.PostCreated =>
          val p = dm.getPost
          val body = p.body.map(b => BodyComponentData(b.component, b.parameters))
          PostCreated(PostData(p.postId, p.key, p.author, p.title, body, p.coverUrl, p.metaTitle, p.metaDescription, p.metaKeywords,
            p.publishedOn, p.commentCount, p.timeToRead, p.tags.toList, p.relatedPosts, p.deleted))
      }
    }

    case class PostPublished(publishedOn: Long) extends PostEvent {
      override def toDataModel: Blog.PostPublished = {
        Blog.PostPublished(publishedOn)
      }
    }

    object PostPublished extends DataModelReader {
      override def fromDataModel: PartialFunction[GeneratedMessage, PostPublished] = {
        case dm: Blog.PostPublished =>
          PostPublished(dm.publishedOn)
      }
    }

    case class RelatedPostAdded(relatedPostIds: Seq[String]) extends PostEvent {
      override def toDataModel: Blog.RelatedPostAdded = {
        Blog.RelatedPostAdded(relatedPostIds)
      }
    }

    object RelatedPostAdded extends DataModelReader {
      override def fromDataModel: PartialFunction[GeneratedMessage, RelatedPostAdded] = {
        case dm: Blog.RelatedPostAdded =>
          RelatedPostAdded(dm.relatedPostIds)
      }
    }

  }

  def props: Props = Props[PostEntity]
}

class PostEntity extends PersistentEntity[PostData] {

  override def additionalCommandHandling: Receive = {
    case CreatePost(post) =>
      persist(PostCreated(post)) {
        handleEventAndRespond()
      }

    case PublishPost(_) =>
      log.info(s"Publishing post ${state.title}")
      persist(PostPublished(new Date().getTime)) {
        handleEventAndRespond()
      }

    case AddRelatedPost(relatedTitle, _) =>
      persist(RelatedPostAdded(state.relatedPosts :+ relatedTitle)) {
        handleEventAndRespond()
      }
  }

  override def isCreateMessage(cmd: Any): Boolean = cmd match {
    case CreatePost(_) => true
    case _ => false
  }

  override def initialState: PostData = PostData.empty

  override def handleEvent(event: EntityEvent): Unit = event match {
    case PostCreated(post) =>
      state = post

    case PostPublished(publishedOn) =>
      state = state.copy(publishedOn = publishedOn)

    case RelatedPostAdded(relatedPostIds) =>
      state = state.copy(relatedPosts = relatedPostIds)
  }
}
