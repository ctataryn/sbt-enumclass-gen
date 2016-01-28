package jp.co.bizreach.enumclassgen.generator

import java.io.File

import jp.co.bizreach.enumclassgen.core.{EnumClass, EnumSettingFileLoader}
import jp.co.bizreach.enumclassgen.setting.EnumClassSetting
import sbt.IO

/**
  * Created by nishiyama on 2016/01/04.
  */
trait EnumPlayJsonWritesSupportGenerator {
  def generateEnumPlaySupportFile(setting: EnumClassSetting): Unit = {
    if (setting.supportPlay) {
      val fileString = generateEnumPlaySupportFileString(setting)
      val output = new File(setting.sourcePath + "/" + setting.packageName.replaceAll("\\.", "/") + "/" + setting.enumClassName + "Implicits.scala")
      IO.write(output, fileString)
    }
  }

  protected def generateEnumPlaySupportFileString(setting: EnumClassSetting): String = {
    val enumClasses = EnumSettingFileLoader.loadEnumClassYaml(new File(setting.enumSettingFile))

    s"""${generatePlaySupportPackageLine(setting)}
       |
       |import play.api.libs.json._
       |
       |${generateImplicitObjects(setting, enumClasses)}
       |""".stripMargin
  }

  protected def generatePlaySupportPackageLine(setting: EnumClassSetting): String = {
    s"package ${setting.packageName}"
  }

  protected def generateImplicitObjects(setting: EnumClassSetting, enumClasses: Seq[EnumClass]): String = {
    s"""object ${setting.enumClassName}Implicits {
       |${enumClasses.map(generateEnumFullImplicits).mkString("\n")}
       |}""".stripMargin
  }

  protected def generateEnumFullImplicits(enumClass: EnumClass): String = {
    s"""  implicit val ${enumClass.name}FullWrites: Writes[${enumClass.name}] = new Writes[${enumClass.name}] {
       |    def writes(enum: ${enumClass.name}) = Json.obj(
       |      "value" -> enum.value${if(enumClass.enumType == "Char")".toString" else ""}
       |${enumClass.attrKeys.map(k => s"""    , "${k.name}" -> enum.${k.name}${if(k.attrType == "Char")".toString" else ""}""").mkString("\n")}
       |    )
       |  }
       |""".stripMargin
  }

  private def enumTypeValue(enumType: String): String = {
    enumType match {
      case "Char" => "v.head"
      case _ => "v"
    }
  }

}