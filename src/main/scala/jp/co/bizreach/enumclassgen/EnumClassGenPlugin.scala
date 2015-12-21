package jp.co.bizreach.enumclassgen

import com.typesafe.config.Config
import jp.co.bizreach.enumclassgen.core.{EnumDocumentGenerator, EnumClassGenerator}
import sbt._
import Keys._
import sbt.complete.Parsers._
import sbt.plugins.CorePlugin

/**
  * Created by nishiyama on 2015/12/09.
  */
object EnumClassGenPlugin extends AutoPlugin
  with EnumClassGenerator with EnumDocumentGenerator {

  object autoImport {
    case class EnumClassSetting(
      enumSettingFile: String,
      sourcePath: String,
      packageName: String,
      enumClassName: String,
      documentPath: Option[String]
    )

    val generateEnumClass = taskKey[Unit]("generate enum classes")

    val enumClassSettings = settingKey[Seq[EnumClassSetting]]("enum settings")

    lazy val baseOfEnumClassMappings: Seq[Def.Setting[_]] = Seq(
      generateEnumClass := {
        enumClassSettings.value.foreach { s =>
          val settingFile = file(s.enumSettingFile)
          if (settingFile.exists) {
            generateEnumClassFile(s)
            generateEnumDocFile(s)
          }
        }
      },
      enumClassSettings in generateEnumClass := Nil
    )
  }

  import autoImport._

  // enablePluginsを書かなくても読み込む。
  override def trigger: PluginTrigger = allRequirements

  // projectSettingsに設定したkeyがこのPluginを利用したProjectにも読み込まれる
  override def projectSettings: Seq[Def.Setting[_]] =
    baseOfEnumClassMappings

}


