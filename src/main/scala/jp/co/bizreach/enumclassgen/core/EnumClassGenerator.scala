package jp.co.bizreach.enumclassgen.core

import java.io.{Closeable, BufferedInputStream, FileInputStream, File}
import java.util

import jp.co.bizreach.enumclassgen.EnumClassGenPlugin.autoImport.EnumClassSetting
import org.yaml.snakeyaml.Yaml
import sbt.IO
import scala.collection.JavaConverters._
import scala.util.Try

/**
  * Created by nishiyama on 2015/12/11.
  */
trait EnumClassGenerator {

  def generateEnumClassFile(setting: EnumClassSetting): Unit = {
    val fileString = generateEnumClassFileString(setting)
    val output = new File(setting.sourcePath + "/" + setting.packageName.replaceAll("\\.", "/") + "/" + setting.enumClassName + ".scala")
    IO.write(output, fileString)
  }

  protected def generateEnumClassFileString(setting: EnumClassSetting): String = {
    val enumClasses = EnumSettingFileLoader.loadEnumClassYaml(new File(setting.enumSettingFile))

    s"""${generatePackageLine(setting)}
       |
       |$generateTraitSealedEnum
       |$generateTraitSealedEnumCompanion
       |
       |${enumClasses.map(generateEnumClassObject).mkString("\n")}
       |""".stripMargin
  }

  protected def generateEnumClassObject(enumClass: EnumClass): String = {
    s"""${generateAbstractSealedClass(enumClass)}
       |${generateEnumObject(enumClass)}
       |""".stripMargin
  }

  protected def generatePackageLine(setting: EnumClassSetting): String = {
    s"package ${setting.packageName}"
  }

  protected def generateTraitSealedEnum: String = {
    s"""/** Base trait for sealed class */
       |trait SealedEnum[+A] {
       |  val value: A
       |}
       |""".stripMargin
  }

  protected def generateTraitSealedEnumCompanion:String = {
    s"""trait SealedEnumCompanion[B <: SealedEnum[A], A] {
       |  val values: Seq[B]
       |  val valueMap: Map[A, B]
       |  def valueOf(value: A): Option[B] = valueMap.get(value)
       |}
       |""".stripMargin
  }

  protected def generateAbstractSealedClass(enumClass: EnumClass): String = {
    val es = enumClass.applyType.map(a => valDefineStr("tpe", a)) +: enumClass.attrKeys.map(k => valDefineStr(k.name, k.attrType, !k.required)).map(a => Some(a))

    val enumClassConstructorString = es.flatten.mkString(", ", ", ", "")
    s"""/** ${enumClass.name}: ${enumClass.description.getOrElse("")} */
       |abstract sealed class ${enumClass.name}(val value: ${enumClass.enumType}$enumClassConstructorString) extends SealedEnum[${enumClass.enumType}]""".stripMargin
  }

  protected def generateEnumValues(enumClass: EnumClass): String = {
    enumClass.values.map(_.name).mkString("val values = Seq(", ", ", ")")
  }

  protected def generateEnumMaps(enumClass: EnumClass): String = {
    enumClass.values.map(v => s"${typeValueStr(enumClass.enumType, v.value)} -> ${v.name}").mkString("val valueMap = Map(", ", ", ")")
  }

  protected def generateEnumValueCaseObject(enumValue: EnumValue, enumClass: EnumClass): String = {
    val enumValueConstructorString =
      (enumValue.applyValue.flatMap( v =>
        enumClass.applyType.map { t =>
          s"tpe = ${typeValueStr(t, v)}"
        }
      ) +:
        enumClass.attrKeys.map { k =>
          valValueStr(k.name, k.attrType, enumValue.attrs.find(_.key.name == k.name).map(_.value), !k.required)
        }.map(v => Some(v))).flatten.mkString(", ", ", ", "")

    s"""|  /** ${enumValue.name}: ${enumValue.comment.getOrElse("")} */
        |  case object ${enumValue.name} extends ${enumClass.name}(${typeValueStr(enumClass.enumType, enumValue.value)}$enumValueConstructorString)
        |""".stripMargin
  }

  protected def generateApplyMethods(enumClass: EnumClass): String = {
    enumClass.applyType.map{ at =>
      val applyMap = s"val applyMap: Map[${at}, ${enumClass.name}] = ${enumClass.values.flatMap(v => v.applyValue.map(a => s"$a -> ${v.name}")).mkString("Map(", ", ", ")")}"
      val applyMethods = s"def apply(tpe: ${at}): ${enumClass.name} = applyMap.get(tpe).get"

      s"""  $applyMap
         |  $applyMethods
         |""".stripMargin
    }.getOrElse("")
  }

  protected def generateEnumObject(enumClass: EnumClass): String = {
    s"""object ${enumClass.name} extends SealedEnumCompanion[${enumClass.name}, ${enumClass.enumType}] {
       |${enumClass.values.map(generateEnumValueCaseObject(_, enumClass)).mkString}
       |
       |  ${generateEnumValues(enumClass)}
       |  ${generateEnumMaps(enumClass)}
       |${generateApplyMethods(enumClass)}
       |}
       |""".stripMargin
  }

  private def valDefineStr(name: String, typeName: String, option: Boolean = false): String = {
    val t = if (option) s"Option[$typeName]" else typeName
    s"val $name: $t"
  }

  private def valValueStr(name: String, valueType: String, value: Option[String], option: Boolean = false): String = {
    val v = if (option) value.map(v => s"Some(${typeValueStr(valueType, v)})").getOrElse("None") else typeValueStr(valueType, value.get)
    s"$name = $v"
  }

  private def typeValueStr(typeString: String, valueString: String): String = {
    typeString match {
      case "Char" => s"'$valueString'"
      case "String" => "\"" + s"${valueString}" + "\""
      case "Long" => s"${valueString}L"
      case _ => valueString
    }
  }

}


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