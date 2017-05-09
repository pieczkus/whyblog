package v1.comment.query

import javax.inject.{Inject, Named}

import akka.actor.{ActorRef, Props}
import akka.persistence.query.EventEnvelope
import akka.stream.ActorMaterializer
import com.sksamuel.elastic4s.ElasticDsl.termQuery
import pl.why.common.ViewBuilder.{InsertAction, UpdateAction}
import pl.why.common.{CommonActor, ElasticSearchSupport, ReadModelObject, ViewBuilder}
import spray.json.JsonFormat
import v1.comment.command.CommentEntity
import v1.comment.command.CommentEntity.Event.{CommentCreated, CommentPublished}
import v1.comment.query.CommentViewBuilder.CommentRM

trait CommentReadModel {
  def indexRoot = "comment"

  def entityType = CommentEntity.EntityType
}

object CommentViewBuilder {
  val Name = "comment-view-builder"

  case class CommentRM(uuid: String, referenceUuid: String, authorName: String, content: String,
                       createdOn: Long = System.currentTimeMillis(), published: Boolean = false) extends ReadModelObject {
    def id: String = uuid
  }

  def props(resumableProjectionManager: ActorRef): Props = Props(new CommentViewBuilder(resumableProjectionManager))
}

class CommentViewBuilder @Inject()(@Named("resumable-projection-manager") rpm: ActorRef)
  extends ViewBuilder[CommentViewBuilder.CommentRM](rpm) with CommentReadModel with CommentJsonProtocol {
  override implicit val rmFormats: JsonFormat[CommentViewBuilder.CommentRM] = warehouseRmFormat

  override def projectionId: String = CommentViewBuilder.Name

  override def actionFor(id: String, env: EventEnvelope): ViewBuilder.IndexAction = env.event match {
    case CommentCreated(c) =>
      val rm = CommentRM(c.uuid, c.referenceUuid, c.authorName, c.content, c.createdOn, c.published)
      InsertAction(id, rm)

    case CommentPublished(_) =>
      UpdateAction(id, Map("published" -> true))
  }
}

object CommentView {
  val Name = "comment-view"

  case class FindComments(referenceUuid: String)

  def props: Props = Props[CommentView]
}

class CommentView extends CommonActor with ElasticSearchSupport with CommentReadModel with CommentJsonProtocol {
  import CommentView._
  import context.dispatcher

  implicit val mater = ActorMaterializer()

  override def receive: Receive = {
    case FindComments(referenceUuid) =>
      pipeResponse(queryElasticSearch(termQuery("referenceUuid.keyword", referenceUuid)))

  }
}
