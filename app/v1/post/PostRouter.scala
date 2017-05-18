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

  }
}
