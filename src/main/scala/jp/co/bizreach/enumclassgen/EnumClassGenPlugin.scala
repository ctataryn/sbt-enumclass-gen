package jp.co.bizreach.enumclassgen

import com.typesafe.config.Config
import jp.co.bizreach.enumclassgen.core.{EnumSlickSupportGenerator, EnumDocumentGenerator, EnumClassGenerator}
import sbt._
import Keys._
import sbt.complete.Parsers._
import sbt.plugins.CorePlugin
import setting._

/**
  * Created by nishiyama on 2015/12/09.
  */
object EnumClassGenPlugin extends AutoPlugin
  with EnumClassGenerator with EnumDocumentGenerator with EnumSlickSupportGenerator {

  object autoImport {

    val generateEnumClass = taskKey[Unit]("generate enum classes")

    val enumClassSettings = settingKey[Seq[EnumClassSetting]]("enum settings")

    lazy val baseOfEnumClassMappings: Seq[Def.Setting[_]] = Seq(
      generateEnumClass := {
        enumClassSettings.value.foreach { s =>
          val settingFile = file(s.enumSettingFile)
          if (settingFile.exists) {
            generateEnumClassFile(s)
            generateEnumDocFile(s)
            generateEnumSlickSupportFile(s)
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


