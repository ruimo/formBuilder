package com.ruimo.forms

import java.nio.file.Files
import java.util

import com.ruimo.scoins.PathUtil
import com.typesafe.config.ConfigFactory
import org.specs2.mutable._

class SettingsSpec extends Specification {
  "Settings" should {
    "can load default value if no file exists." in {
      PathUtil.withTempDir(prefix = None) { dir =>
        val loader = new SettingsLoader(dir.resolve("foo.conf"))
        val settings = loader.settings
        settings.auth === NullAuthSettings
      }.get
    }

    "can load value." in {
      PathUtil.withTempDir(prefix = None) { dir =>
        val confFile = dir.resolve("foo.conf")
        Files.write(
          confFile,
          util.Arrays.asList(
            "auth {",
            "  userName = userName0",
            "  applicationToken = applicationToken0",
            "  url = url0",
            "}"
          )
        )
        val loader = new SettingsLoader(confFile)
        val settings = loader.settings
        settings.auth.contractedUserId === ContractedUserId("userName0")
        settings.auth.applicationToken === ApplicationToken("applicationToken0")
        settings.auth.url === Url("url0")
      }.get
    }

    "can load default value and save new value." in {
      PathUtil.withTempDir(prefix = None) { dir =>
        val confFile = dir.resolve("foo.conf")
        val loader = new SettingsLoader(confFile)
        loader.update(
          Settings(
            AuthSettingsImpl(
              ContractedUserId("userName0"), ApplicationToken("applicationToken0"), Url("url0")
            )
          )
        )
        val conf = ConfigFactory.parseFile(confFile.toFile)
        conf.getString("auth.userName") === "userName0"
        conf.getString("auth.applicationToken") === "applicationToken0"
        conf.getString("auth.url") === "url0"
      }.get
    }

    "can load value and save new value." in {
      PathUtil.withTempDir(prefix = None) { dir =>
        val confFile = dir.resolve("foo.conf")
        Files.write(
          confFile,
          util.Arrays.asList(
            "auth {",
            "  userName = userName0",
            "  applicationToken = applicationToken0",
            "  url = url0",
            "}"
          )
        )
        val loader = new SettingsLoader(confFile)
        val settings = loader.settings
        loader.update(
          settings.copy(
            auth = AuthSettingsImpl(
              ContractedUserId("userName1"), ApplicationToken("applicationToken1"), Url("url1")
            )
          )
        )
        val conf = ConfigFactory.parseFile(confFile.toFile)
        conf.getString("auth.userName") === "userName1"
        conf.getString("auth.applicationToken") === "applicationToken1"
        conf.getString("auth.url") === "url1"
      }.get
    }
  }
}
