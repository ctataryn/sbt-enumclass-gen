package jp.co.bizreach.enumclassgen.generator

import java.io.File

import jp.co.bizreach.enumclassgen.core.{EnumClass, EnumSettingFileLoader, EnumValue}
import jp.co.bizreach.enumclassgen.setting.EnumClassSetting
import sbt.IO

/**
  * Created by nishiyama on 2016/01/04.
  */
trait EnumDocumentGenerator {

  def generateEnumDocFile(setting: EnumClassSetting): Unit = {
    setting.documentPath.foreach{ path =>
      val fileString = generateEnumDocumentFileString(setting)
      val output = new File(path)
      IO.write(output, fileString)
    }
  }

  protected def generateEnumDocumentFileString(setting: EnumClassSetting): String = {
    val enumClasses = EnumSettingFileLoader.loadEnumClassYaml(new File(setting.enumSettingFile))


    s"""${generateTitle(setting)}
       |
       |${generateSettingInfo(setting)}
       |
       |${enumClasses.map(generateEnumClassDocument).mkString("\n")}
       |""".stripMargin
  }

  protected def generateTitle(setting: EnumClassSetting): String = {
    s"# ${setting.enumClassName}"
  }

  protected def generateSettingInfo(setting: EnumClassSetting): String = {
    s"""- File: `${setting.enumSettingFile}`
        |- Source: `${setting.sourcePath}`
        |- Class: `${setting.packageName}.${setting.enumClassName}`
        |""".stripMargin
  }

  protected def generateEnumClassDocument(enumClass: EnumClass): String = {
    s"""## ${enumClass.name}
        |
       |```
        |${enumClass.description.getOrElse("")}
        |```
        |
       |- type: `${enumClass.enumType}`
        |${generateAttributesDocument(enumClass)}
        |${enumClass.applyType.map(a => s"- applyType: `$a`").getOrElse("")}
        |
       |${generateValuesDocument(enumClass)}
        |""".stripMargin
  }

  protected def generateAttributesDocument(enumClass: EnumClass): String = {
    s"""- attributes
        |${enumClass.attrKeys.map(k => s" - `${k.name}` : ${k.attrType}${if (!k.required) "(optional)" else ""}").mkString("\n")}
        |
       |""".stripMargin
  }

  protected def generateValuesDocument(enumClass: EnumClass): String = {
    s"""### Values
        |
       |${pipe}Name|Value|Comment|${enumClass.attrKeys.map(k => s"${k.name}${if (!k.required) "(optional)" else ""}").mkString("|")}|${enumClass.applyType.map(a => s"$a|").getOrElse("")}
        |${pipe}----|-----|-------|${enumClass.attrKeys.map(k => "----").mkString("|")}|${enumClass.applyType.map(a => "----|").getOrElse("")}
        |${enumClass.values.map(v => generateValueDocument(enumClass, v)).mkString("\n")}
     """.stripMargin
  }


  protected def generateValueDocument(enumClass: EnumClass, enumValue: EnumValue): String = {
    s"|${pipe}${enumValue.name}|${enumValue.value}|${enumValue.comment.getOrElse(" ")}|${enumClass.attrKeys.map(k => enumValue.attrs.find(_.key.name == k.name).map(v => v.value).getOrElse(" ")).mkString("|")}|${enumClass.applyType.map(a => enumValue.applyValue.getOrElse(" ") + "|").getOrElse("")}"
  }

  protected val pipe = "|"
}