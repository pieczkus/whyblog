package v1.post.read

import pl.why.common.BaseJsonProtocol
import spray.json.RootJsonFormat
import v1.post.command.BodyComponentData
import v1.post.read.PostViewBuilder.PostRM

trait PostJsonProtocol extends BaseJsonProtocol {

  implicit val bodyComponentFormat: RootJsonFormat[BodyComponentData] = jsonFormat2(BodyComponentData.apply)

  implicit val postRmFormat: RootJsonFormat[PostRM] = jsonFormat17(PostRM.apply)

}
