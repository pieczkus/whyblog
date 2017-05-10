package v1.post.read

import javax.inject.{Inject, Named}

import akka.actor.{ActorRef, Props}
import akka.persistence.query.EventEnvelope
import akka.stream.ActorMaterializer
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.searches.queries.{QueryDefinition, RangeQueryDefinition}
import com.sksamuel.elastic4s.searches.sort.FieldSortDefinition
import org.elasticsearch.search.sort.SortOrder
import pl.why.common.ViewBuilder.{InsertAction, UpdateAction}
import pl.why.common.{CommonActor, ElasticSearchSupport, ReadModelObject, ViewBuilder}
import spray.json.JsonFormat
import v1.post.command.PostEntity.Event.{PostCreated, PostPublished}
import v1.post.command.{BodyComponentData, PostEntity}
import v1.post.read.PostViewBuilder.PostRM

trait CommentReadModel {
  def indexRoot = "post"

  def entityType = PostEntity.EntityType
}

object PostViewBuilder {
  val Name = "post-view-builder"

  case class PostRM(postId: String, key: String, author: String, title: String, body: Seq[BodyComponentData], coverUrl: String,
                    metaTitle: String, metaDescription: String, metaKeywords: String, publishedOn: Long = 0L,
                    commentCount: Int = 0, timeToRead: String = "", tags: List[String] = List.empty,
                    relatedPosts: Seq[String] = List.empty, deleted: Boolean = false) extends ReadModelObject {
    def id: String = postId
  }

  def props(resumableProjectionManager: ActorRef): Props = Props(new PostViewBuilder(resumableProjectionManager))
}

class PostViewBuilder @Inject()(@Named("resumable-projection-manager") rpm: ActorRef)
  extends ViewBuilder[PostViewBuilder.PostRM](rpm) with CommentReadModel with PostJsonProtocol {
  override implicit val rmFormats: JsonFormat[PostViewBuilder.PostRM] = postRmFormat

  override def projectionId: String = PostViewBuilder.Name

  override def actionFor(id: String, env: EventEnvelope): ViewBuilder.IndexAction = env.event match {
    case PostCreated(p) =>
      val rm = PostRM(p.postId, p.key, p.author, p.timeToRead, p.body, p.coverUrl, p.metaTitle, p.metaDescription, p.metaKeywords,
        p.publishedOn, p.commentCount, p.timeToRead, p.tags, p.relatedPosts, p.deleted)
      InsertAction(id, rm)

    case PostPublished(_) =>
      UpdateAction(id, Map("published" -> true))
  }
}

object PostView {
  val Name = "post-view"

  case class FindPostByTitle(key: String, title: String)

  case class FindPostsByTag(key: String, tag: String)

  case class FindPostPublishedAfter(key: String, after: Long)

  case class FindPostPublishedBefore(key: String, before: Long)

  //case class FindPublishedPosts(key: String, page: Option[PageRequest])

  def props: Props = Props[PostView]
}

class PostView extends CommonActor with ElasticSearchSupport with CommentReadModel with PostJsonProtocol {

  import PostView._
  import context.dispatcher

  implicit val mater = ActorMaterializer()

  lazy val defaultSort = FieldSortDefinition("publishedOn", order = SortOrder.DESC)

  def defaultPublishedQuery(key: String): QueryDefinition = boolQuery().must(rangeQuery("publishedOn").gte(1L.toString), termQuery("key.keyword", key))

  override def receive: Receive = {
    case FindPostByTitle(key, title) =>
      pipeResponse(queryElasticSearch[PostRM](boolQuery().must(defaultPublishedQuery(key), termQuery("title.keyword", title)), sort = Some(defaultSort)))

    case FindPostsByTag(key, tag) =>
      pipeResponse(queryElasticSearch[PostRM](boolQuery().must(defaultPublishedQuery(key)).filter(matchQuery("tags", tag)), sort = Some(defaultSort)))

    case FindPostPublishedAfter(key, after) =>
      pipeResponse(queryElasticSearch[PostRM](boolQuery().must(defaultPublishedQuery(key), rangeQuery("publishedOn").from(after).includeLower(false)), size = Some(1)))

    case FindPostPublishedBefore(key, before) =>
      pipeResponse(queryElasticSearch[PostRM](boolQuery().must(defaultPublishedQuery(key), rangeQuery("publishedOn").to(before).includeLower(false)), size = Some(1)))
  }
}
