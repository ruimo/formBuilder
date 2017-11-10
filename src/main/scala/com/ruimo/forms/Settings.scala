package com.ruimo.forms

import java.io.{BufferedReader, FileReader}
import java.nio.file.{Files, Path, Paths}

import com.ruimo.scoins.LoanPattern
import play.api.libs.json.{JsBoolean, JsObject, JsString, JsValue, Json}
import com.typesafe.config.{Config, ConfigFactory, ConfigObject, ConfigValueFactory}

import scala.annotation.tailrec
import scala.collection.JavaConverters._

case class UserName(value: String) extends AnyVal
case class ApplicationToken(value: String) extends AnyVal
trait Url {
  def resolve(childPath: String): String
  def value: String
}
object Url {
  def apply(value: String): Url = {
    @tailrec def chop(path: String): String =
      if (path.endsWith("/")) chop(path.substring(0, path.length() - 1)) else path

    UrlImpl(chop(value))
  }
}

private case class UrlImpl(value: String) extends Url {
  def resolve(childPath: String): String = {
    if (childPath.startsWith("/"))
      throw new IllegalArgumentException("childpath '" + childPath + "' should not starts with /.")

    value + "/" + childPath
  }
}

case class Settings(
  auth: AuthSettings
) {
  lazy val asJson: JsValue = Json.obj(
    "auth" -> auth.asJson
  )

  lazy val asConf: ConfigObject = ConfigValueFactory.fromMap(
    Map(
      "auth" -> auth.asConf
    ).asJava
  )
}

sealed trait AuthSettings {
  def asJson: JsValue
  def asConf: ConfigObject
  def userName: UserName
  def applicationToken: ApplicationToken
  def url: Url
}

case object NullAuthSettings extends AuthSettings {
  def asJson: JsValue = throw new RuntimeException
  def asConf: ConfigObject = throw new RuntimeException
  def userName: UserName = throw new RuntimeException
  def applicationToken: ApplicationToken = throw new RuntimeException
  def url: Url = throw new RuntimeException
}

case class AuthSettingsImpl(
  userName: UserName,
  applicationToken: ApplicationToken,
  url: Url
) extends AuthSettings {
  val asJson: JsValue = Json.obj(
    "userName" -> userName.value,
    "applicationToken" -> applicationToken.value,
    "url" -> url.value
  )

  val asConf: ConfigObject = ConfigValueFactory.fromMap(
    Map(
      "userName" -> userName.value,
      "applicationToken" -> applicationToken.value,
      "url" -> url.value
    ).asJava
  )
}

class SettingsLoader(configFile: Path) {
  @volatile private[this] var instance: Settings =
    if (! Files.exists(configFile)) Settings.Null
    else load(ConfigFactory.parseFile(configFile.toFile))

  def settings: Settings = instance

  private def load(config: Config): Settings = Settings(
    AuthSettings.load(config.getConfig("auth"))
  )

  private def save(settings: Settings): Unit = {
    println("Save to '" + configFile.toAbsolutePath + "'")
    Files.write(configFile, settings.asConf.render().getBytes("utf-8"))
  }

  def update(settings: Settings): Unit = {
    this.instance = settings
    save(settings)
  }
}

object Settings {
  val FileName = ".formBuilder.conf"
  val ConfPath = Paths.get(System.getProperty("user.home")).resolve(FileName)
  val Null = Settings(NullAuthSettings)
  val Loader = new SettingsLoader(ConfPath)
}

object AuthSettings {
  def load(config: Config): AuthSettings = {
    AuthSettingsImpl(
      UserName(config.getString("userName")),
      ApplicationToken(config.getString("applicationToken")),
      Url(config.getString("url"))
    )
  }
}
