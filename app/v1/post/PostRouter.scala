package v1.post

import javax.inject.Inject

import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class PostRouter @Inject()(controller: PostController) extends SimpleRouter {

  override def routes: Routes = {

    case GET(p"/title/$title") =>
      controller.findByTitle(title)

    case POST(p"/title/$title") =>
      controller.publish(title)

    case POST(p"/") =>
      controller.create

    case GET(p"/") =>
      controller.find

    case GET(p"/pinned") =>
      controller.findPinnedPost

    case POST(p"/title/$title/pin") =>
      controller.pinPost(title)

    case GET(p"/notpublished") =>
      controller.findNotPublished

    case GET(p"/tag/$tag") =>
      controller.findByTag(tag)

    case GET(p"/after/${long(after)}") =>
      controller.findAfter(after)

    case GET(p"/before/${long(before)}") =>
      controller.findBefore(before)

    case POST(p"/${int(postId)}/related/${int(relatedId)}") =>
      controller.addRelated(postId.toString, relatedId.toString)

    case GET(p"/${int(postId)}") =>
      controller.find(postId.toString)
  }
}
