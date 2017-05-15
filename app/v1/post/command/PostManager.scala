package v1.post.command

import akka.actor.Props
import akka.util.Timeout
import pl.why.common.PersistentEntity.GetState
import pl.why.common._
import v1.post.AddPostInput
import v1.post.command.PostEntity.Command.{AddRelatedPost, CreatePost, PublishPost}

import scala.concurrent.duration._


object PostManager {
  val Name = "post-manager"

  case class FindPostById(id: String)

  case class AddPost(key: String, input: AddPostInput)

  case class AnnouncePost(key: String, title: String)

  val TitleNotUniqueError = ErrorMessage("post.title.nonunique", Some("The post title supplied for a create is not unique"))

  def props: Props = Props[PostManager]

}

class PostManager extends Aggregate[PostData, PostEntity] {

  import PostManager._
  import akka.pattern.ask
  import context.dispatcher

  override def entityProps: Props = PostEntity.props

  override def receive: Receive = {
    case FindPostById(id) =>
      forwardCommand(id, GetState(id))

    case AddPost(key, input) =>
      implicit val timeout = Timeout(5.seconds)

      val id = getPostId(key, input.title)

      val stateFut = (entityShardRegion ? GetState(id)).mapTo[ServiceResult[PostData]]
      val caller = sender()
      stateFut onComplete {
        case util.Success(FullResult(_)) =>
          caller ! Failure(FailureType.Validation, TitleNotUniqueError)

        case util.Success(EmptyResult) =>

          val body = input.body.map(c => BodyComponentData(c.component, c.parameters.toMap))
          val fo = PostData(id, key, input.author, input.title, body, input.coverUrl, input.metaTitle, input.metaDescription,
            input.metaKeywords, tags = input.tags, timeToRead = calculateTieToRead(body))
          entityShardRegion.tell(CreatePost(fo), caller)

        case _ =>
          caller ! Failure(FailureType.Service, ServiceResult.UnexpectedFailure)
      }

    case AnnouncePost(key, title) =>
      val postId = getPostId(key, title)
      forwardCommand(postId, PublishPost(postId))

    case arp: AddRelatedPost =>
      forwardCommand(arp.id, arp)
  }

  private def calculateTieToRead(body: Seq[BodyComponentData]): String = {
    val minutes = body.filter(bc => bc.parameters.values.toList.contains("text"))
      .flatMap(bc => bc.parameters.filter(p => p._1.eq("text")))
      .map(p => p._2.split("\\W+").length).sum
    if (minutes <= 1) {
      "1 Minuta"
    } else if (minutes > 10) {
      "10+ Minut"
    } else {
      minutes + " Minuty"
    }
  }

  private def getPostId(key: String, title: String): String = {
    Math.abs((key + "-" + title).hashCode).toString
  }
}
