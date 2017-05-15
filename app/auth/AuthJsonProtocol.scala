package auth

import auth.AuthService.ValidationResult
import pl.why.common.lookup.ApiResponseJsonProtocol
import spray.json.RootJsonFormat

trait AuthJsonProtocol extends ApiResponseJsonProtocol {

  implicit val validationResultFormat: RootJsonFormat[ValidationResult] = jsonFormat1(ValidationResult.apply)

}
