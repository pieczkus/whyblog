package auth

import auth.AuthService.ValidationResult
import pl.why.common.BaseJsonProtocol
import spray.json.RootJsonFormat

trait AuthJsonProtocol extends BaseJsonProtocol {

  implicit val validationResultFormat: RootJsonFormat[ValidationResult] = jsonFormat1(ValidationResult.apply)

}
