import javax.inject._

import auth.AuthService
import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import pl.why.common.resumable.ResumableProjectionManager
import play.api.libs.concurrent.AkkaGuiceSupport
import play.api.{Configuration, Environment}
import v1.post.command.PostManager
import v1.post.read.PostView

class Module (environment: Environment, configuration: Configuration)
  extends AbstractModule
    with ScalaModule with AkkaGuiceSupport {

  override def configure() = {

    bindActor[ResumableProjectionManager](ResumableProjectionManager.Name)

    bindActor[PostManager](PostManager.Name)

    bindActor[AuthService](AuthService.Name)

    bindActor[PostView](PostView.Name)

    bind(classOf[ClusterSingleton]).asEagerSingleton()
  }
}
