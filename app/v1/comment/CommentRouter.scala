package v1.comment

import javax.inject.Inject

import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class CommentRouter @Inject()(controller: CommentController) extends SimpleRouter {
  val prefix = "/v1/devices"

  override def routes: Routes = {

    case GET(p"/$uuid") =>
      controller.find(uuid)

    case POST(p"/") =>
      controller.create

  }
}