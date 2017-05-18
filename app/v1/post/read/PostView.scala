package v1.post.read

import javax.inject.{Inject, Named}

import akka.actor.{ActorRef, Props}
import akka.persistence.query.EventEnvelope
import akka.stream.ActorMaterializer
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.searches.queries.QueryDefinition
import com.sksamuel.elastic4s.searches.sort.FieldSortDefinition
import org.elasticsearch.search.sort.SortOrder
import pl.why.common.ViewBuilder.{InsertAction, UpdateAction}
import pl.why.common.{CommonActor, ElasticSearchSupport, ReadModelObject, ViewBuilder}
import spray.json.JsonFormat
import v1.post.command.PostEntity.Event.{PostCreated, PostPinToggled, PostPublished}
import v1.post.command.{BodyComponentData, PostEntity}
import v1.post.read.PostViewBuilder.PostRM

trait PostReadModel {
  def indexRoot = "post"

  def entityType = PostEntity.EntityType
}

object PostViewBuilder {
  val Name = "post-view-builder"

  case class PostRM(postId: String, key: String, author: String, title: String, body: Seq[BodyComponentData], coverUrl: String,
                    metaTitle: String, metaDescription: String, metaKeywords: String, publishedOn: Long = 0L,
                    commentCount: Int = 0, timeToRead: String = "", tags: List[String] = List.empty,
                    relatedPosts: Seq[String] = List.empty, pinned: Boolean = false, published: Boolean = false, deleted: Boolean = false) extends ReadModelObject {
    def id: String = postId
  }

  def props(resumableProjectionManager: ActorRef): Props = Props(new PostViewBuilder(resumableProjectionManager))
}

class PostViewBuilder @Inject()(@Named("resumable-projection-manager") rpm: ActorRef)
  extends ViewBuilder[PostViewBuilder.PostRM](rpm) with PostReadModel with PostJsonProtocol {
  override implicit val rmFormats: JsonFormat[PostViewBuilder.PostRM] = postRmFormat

  override def projectionId: String = PostViewBuilder.Name

  override def actionFor(id: String, env: EventEnvelope): ViewBuilder.IndexAction = env.event match {
    case PostCreated(p) =>
      val rm = PostRM(p.postId, p.key, p.author, p.title, p.body, p.coverUrl, p.metaTitle, p.metaDescription, p.metaKeywords,
        p.publishedOn, p.commentCount, p.timeToRead, p.tags, p.relatedPosts, p.pinned, p.deleted)
      InsertAction(id, rm)

    case PostPublished(publishedOn) =>
      UpdateAction(id, Map("published" -> true, "publishedOn" -> publishedOn))

    case PostPinToggled(pinned) =>
      UpdateAction(id, Map("pinned" -> pinned))
  }
}

object PostView {
  val Name = "post-view"

  case class FindPostByTitle(key: String, title: String)

  case class FindPinnedPost(key: String)

  case class FindPostsByTag(key: String, tag: String)

  case class FindPostPublishedAfter(key: String, after: Long)

  case class FindPostPublishedBefore(key: String, before: Long)

  case class FindPublishedPosts(key: String, page: Option[PageRequest])

  case class FindNotPublishedPosts(key: String, page: Option[PageRequest])

  def props: Props = Props[PostView]
}

class PostView extends CommonActor with ElasticSearchSupport with PostReadModel with PostJsonProtocol {

  import PostView._
  import context.dispatcher

  implicit val mater = ActorMaterializer()

  lazy val defaultSort = FieldSortDefinition("publishedOn", order = SortOrder.DESC)

  lazy val defaultPublishedQuery: QueryDefinition = termQuery("published", true)
  lazy val defaultNotPublishedQuery: QueryDefinition = termQuery("published", false)

  def defaultKeyQuery(key: String): QueryDefinition = termQuery("key.keyword", key)

  override def receive: Receive = {
    case FindPostByTitle(key, title) =>
      pipeResponse(queryElasticSearch[PostRM](boolQuery().must(defaultPublishedQuery, defaultKeyQuery(key), termQuery("title.keyword", title))))

    case FindPostsByTag(key, tag) =>
      pipeResponse(queryElasticSearch[PostRM](boolQuery().must(defaultKeyQuery(key)).filter(matchQuery("tags", tag)), sort = Some(defaultSort)))

    case FindPublishedPosts(key, None) =>
      pipeResponse(queryElasticSearch[PostRM](boolQuery().must(defaultKeyQuery(key), defaultPublishedQuery), sort = Some(defaultSort)))

    case FindNotPublishedPosts(key, None) =>
      pipeResponse(queryElasticSearch[PostRM](boolQuery().must(defaultKeyQuery(key), defaultNotPublishedQuery), sort = Some(defaultSort)))

    case FindPostPublishedAfter(key, after) =>
    //pipeResponse(queryElasticSearch[PostRM](boolQuery().must(defaultPublishedQuery(key), rangeQuery("publishedOn").from(after).includeLower(false)), size = Some(1)))

    case FindPostPublishedBefore(key, before) =>
    //pipeResponse(queryElasticSearch[PostRM](boolQuery().must(defaultPublishedQuery(key), rangeQuery("publishedOn").to(before).includeLower(false)), size = Some(1)))

    case FindPinnedPost(key) =>
      pipeResponse(queryElasticSearch[PostRM](boolQuery().must(defaultPublishedQuery, defaultKeyQuery(key), termQuery("pinned", true))))
  }
}
