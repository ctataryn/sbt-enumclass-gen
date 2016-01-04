package jp.co.bizreach.enumclassgen.generator

import java.io.File

import jp.co.bizreach.enumclassgen.core.{EnumClass, EnumSettingFileLoader}
import jp.co.bizreach.enumclassgen.setting.EnumClassSetting
import sbt.IO

/**
  * Created by nishiyama on 2016/01/04.
  */
trait EnumPlaySupportGenerator {
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
       |import play.api.data.validation.ValidationError
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
       |
       |${enumClasses.map(generateEnumImplicits).mkString("\n")}
       |
       |}""".stripMargin
  }

  protected def generateEnumImplicits(enumClass: EnumClass): String = {
    s"""  implicit val ${enumClass.name}Writes: Writes[${enumClass.name}] = new Writes[${enumClass.name}] {
       |    def writes(enum: ${enumClass.name}) = Json.toJson(enum.value.toString)
       |  }
       |
       |  implicit val ${enumClass.name}Reads: Reads[${enumClass.name}] = new Reads[${enumClass.name}] {
       |    def reads(json: JsValue): JsResult[${enumClass.name}] = {
       |      json.validateOpt[String].fold[JsResult[${enumClass.name}]](
       |        invalid => JsError(invalid),
       |        valid => valid.flatMap(v => ${enumClass.name}.valueOf(${enumTypeValue(enumClass.enumType)})).fold[JsResult[${enumClass.name}]](
       |          JsError(__, ValidationError("validate.error.missing", "${enumClass.name}"))
       |        )(validValue => JsSuccess(validValue))
       |      )
       |    }
       |  }
       |
       |""".stripMargin
  }

  private def enumTypeValue(enumType: String): String = {
    enumType match {
      case "Char" => "v.head"
      case _ => "v"
    }
  }

}