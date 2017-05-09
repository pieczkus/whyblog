package v1.post.command

import akka.actor.Props
import akka.util.Timeout
import pl.why.common.PersistentEntity.GetState
import pl.why.common._
import v1.post.command.PostEntity.Command.{AddRelatedPost, CreatePost, PublishPost}

import scala.concurrent.duration._


object PostManager {
  val Name = "posts-manager"

  case class FindPostById(id: String)

  case class AddPostInput(key: String, author: String, title: String, body: Seq[BodyComponentData], coverUrl: String, tags: List[String],
                          metaTitle: String, metaDescription: String, metaKeywords: String)

  case class AddPost(input: AddPostInput)

  case class AnnouncePost(uuid: String)

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

    case AddPost(input) =>
      implicit val timeout = Timeout(5.seconds)

      val id = Math.abs(input.title.hashCode).toString

      val stateFut = (entityShardRegion ? GetState(id)).mapTo[ServiceResult[PostData]]
      val caller = sender()
      stateFut onComplete {
        case util.Success(FullResult(_)) =>
          caller ! Failure(FailureType.Validation, TitleNotUniqueError)

        case util.Success(EmptyResult) =>
          val fo = PostData(id, input.key, input.author, input.title, input.body, input.coverUrl, input.metaTitle, input.metaDescription,
            input
            .metaKeywords, tags = input.tags, timeToRead = calculateTieToRead(input.body))
          entityShardRegion.tell(CreatePost(fo), caller)

        case _ =>
          caller ! Failure(FailureType.Service, ServiceResult.UnexpectedFailure)
      }

    case AnnouncePost(postId) =>
      forwardCommand(postId, PublishPost(postId))

    case arp: AddRelatedPost =>
      forwardCommand(arp.id, arp)
  }

  private def calculateTieToRead(body: Seq[BodyComponentData]): String = {
    val minutes = body.filter(bc => bc.parameters.contains("text")).map(bc => bc.parameters("key").split("\\W+").length).sum
    if (minutes <= 1) {
      "1 Minuta"
    } else if (minutes > 10) {
      "10+ Minut"
    } else {
      minutes + " Minuty"
    }
  }
}
