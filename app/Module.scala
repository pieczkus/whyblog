import javax.inject._

import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import pl.why.common.resumable.ResumableProjectionManager
import play.api.libs.concurrent.AkkaGuiceSupport
import play.api.{Configuration, Environment}
import v1.post.command.PostManager

class Module (environment: Environment, configuration: Configuration)
  extends AbstractModule
    with ScalaModule with AkkaGuiceSupport {

  override def configure() = {

    bindActor[ResumableProjectionManager](ResumableProjectionManager.Name)

    bindActor[PostManager](PostManager.Name)

    bind(classOf[ClusterSingleton]).asEagerSingleton()
  }
}
