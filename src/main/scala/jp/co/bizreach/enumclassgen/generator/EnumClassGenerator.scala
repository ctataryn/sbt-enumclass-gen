package jp.co.bizreach.enumclassgen.generator

import java.io.File

import jp.co.bizreach.enumclassgen.core.{EnumClass, EnumSettingFileLoader, EnumValue}
import jp.co.bizreach.enumclassgen.setting.EnumClassSetting
import sbt.IO

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
    val es = (enumClass.applyType.map(a => valDefineStr("tpe", a)) +: enumClass.attrKeys.map(k => valDefineStr(k.name, k.attrType, !k.required)).map(a => Some(a))).flatten

    val enumClassConstructorString = if (es.nonEmpty) es.mkString(", ", ", ", "") else ""
    s"""/** ${enumClass.name}: ${enumClass.description.getOrElse("")} */
       |abstract sealed class ${enumClass.name}(val value: ${enumClass.enumType}$enumClassConstructorString) extends SealedEnum[${enumClass.enumType}] { val v: ${enumClass.name} = this }""".stripMargin
  }

  protected def generateEnumValues(enumClass: EnumClass): String = {
    enumClass.values.map(_.name).mkString("val values = Seq(", ", ", ")")
  }

  protected def generateEnumMaps(enumClass: EnumClass): String = {
    enumClass.values.map(v => s"${typeValueStr(enumClass.enumType, v.value)} -> ${v.name}").mkString("val valueMap = Map(", ", ", ")")
  }

  protected def generateEnumValueCaseObject(enumValue: EnumValue, enumClass: EnumClass): String = {
    val enumValueConstructorString = {
      val attrValues = (enumValue.applyValue.flatMap(v =>
        enumClass.applyType.map { t =>
          s"tpe = ${typeValueStr(t, v)}"
        }
      ) +:
        enumClass.attrKeys.map { k =>
          valValueStr(k.name, k.attrType, enumValue.attrs.find(_.key.name == k.name).map(_.value), !k.required)
        }.map(v => Some(v))).flatten
      if (attrValues.nonEmpty) attrValues.mkString(", ", ", ", "") else ""
    }

    s"""|  /** ${enumValue.name}: ${enumValue.comment.getOrElse("")} */
        |  case object ${enumValue.name} extends ${enumClass.name}(${typeValueStr(enumClass.enumType, enumValue.value)}$enumValueConstructorString)
        |""".stripMargin
  }

  protected def generateApplyTpeMethods(enumClass: EnumClass): String = {
    enumClass.applyType.map{ at =>
      val applyMap = s"val tpeMap: Map[${at}, ${enumClass.name}] = ${enumClass.values.flatMap(v => v.applyValue.map(a => s"$a -> ${v.name}")).mkString("Map(", ", ", ")")}"
      val applyMethods = s"def applyTpe(tpe: ${at}): ${enumClass.name} = tpeMap.get(tpe).get"

      s"""  $applyMap
         |  $applyMethods
         |""".stripMargin
    }.getOrElse("")
  }

  protected def generateApplyMethods(enumClass: EnumClass): String = {
    s"def apply(value: ${enumClass.enumType}): ${enumClass.name} = valueMap.getOrElse(value, values.head)"
  }

  protected def generateEnumObject(enumClass: EnumClass): String = {
    s"""object ${enumClass.name} extends SealedEnumCompanion[${enumClass.name}, ${enumClass.enumType}] {
       |${enumClass.values.map(generateEnumValueCaseObject(_, enumClass)).mkString}
       |
       |  ${generateEnumValues(enumClass)}
       |  ${generateEnumMaps(enumClass)}
       |  ${generateApplyMethods(enumClass)}
       |${generateApplyTpeMethods(enumClass)}
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


