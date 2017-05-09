package v1.comment.query

import pl.why.common.BaseJsonProtocol
import spray.json.RootJsonFormat
import v1.comment.query.CommentViewBuilder.CommentRM

trait CommentJsonProtocol extends BaseJsonProtocol {

  implicit val warehouseRmFormat: RootJsonFormat[CommentRM] = jsonFormat6(CommentRM.apply)

}
