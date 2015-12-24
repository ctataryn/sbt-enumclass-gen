package jp.co.bizreach.enumclassgen.core

import java.io.{Closeable, BufferedInputStream, FileInputStream, File}
import java.util

import jp.co.bizreach.enumclassgen.setting.EnumClassSetting
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
    val es = (enumClass.applyType.map(a => valDefineStr("tpe", a)) +: enumClass.attrKeys.map(k => valDefineStr(k.name, k.attrType, !k.required)).map(a => Some(a))).flatten

    val enumClassConstructorString = if (es.nonEmpty) es.mkString(", ", ", ", "") else ""
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

trait EnumSlickSupportGenerator {
  def generateEnumSlickSupportFile(setting: EnumClassSetting): Unit = {
    if (setting.slickSupport) {
      val fileString = generateEnumSlickSupportFileString(setting)
      val output = new File(setting.sourcePath + "/" + setting.packageName.replaceAll("\\.", "/") + "/" + setting.enumClassName + "Support.scala")
      IO.write(output, fileString)
    }
  }

  protected def generateEnumSlickSupportFileString(setting: EnumClassSetting): String = {
    val enumClasses = EnumSettingFileLoader.loadEnumClassYaml(new File(setting.enumSettingFile))

    s"""${generateSlickSupportPackageLine(setting)}
       |
       |import java.sql.{PreparedStatement, ResultSet}
       |import slick.jdbc.{SetParameter, PositionedParameters, GetResult, PositionedResult}
       |import slick.driver._
       |
       |${generateTraits(setting)}
       |${enumClasses.map(generateEnumClassMapper(_)(setting)).mkString("\n")}
       |
       |${generateEnumClassMapperSupport(enumClasses)(setting)}
       |""".stripMargin
  }

  protected def generateSlickSupportPackageLine(setting: EnumClassSetting): String = {
    s"package ${setting.packageName}"
  }

  protected def generateTraits(setting: EnumClassSetting): String = {
    s"""trait ${setting.enumClassName}FromTypeConverter[A, B] {
       |  def fromSqlType(a: A): B
       |}
       |
       |trait ${setting.enumClassName}ToTypeConverter[A, B] {
       |  def toSqlType(b: B): A
       |}
       |
       |trait ${setting.enumClassName}SqlTypeConverter[A, B] extends ${setting.enumClassName}FromTypeConverter[A, B] with ${setting.enumClassName}ToTypeConverter[A, B]
       |
       |trait ${setting.enumClassName}GetResult[A, B] { self: ${setting.enumClassName}SqlTypeConverter[A, B] =>
       |
       |  def next(rs: PositionedResult): A
       |  def nextOption(rs: PositionedResult): Option[A]
       |
       |  object getResult extends GetResult[B] {
       |    def apply(rs: PositionedResult) = fromSqlType(next(rs))
       |  }
       |  object getOptionResult extends GetResult[Option[B]] {
       |    def apply(rs: PositionedResult) = nextOption(rs).map(fromSqlType)
       |  }
       |}
       |
       |trait ${setting.enumClassName}SetParameter[A, B] { self: ${setting.enumClassName}SqlTypeConverter[A, B] =>
       |
       |  def set(rs: PositionedParameters, d: A): Unit
       |  def setOption(rs: PositionedParameters, d: Option[A]): Unit
       |
       |  object setParameter extends SetParameter[B] {
       |    def apply(d: B, p: PositionedParameters): Unit = set(p, toSqlType(d))
       |  }
       |
       |  object setOptionParameter extends SetParameter[Option[B]] {
       |    def apply(d: Option[B], p: PositionedParameters): Unit = setOption(p, d.map(toSqlType))
       |  }
       |}
       |
       |class ${setting.enumClassName}MappingNotFoundException[A](message: String, value: A) extends Exception(message)
       |""".stripMargin
  }

  protected def generateEnumClassMapper(enumClass: EnumClass)(setting: EnumClassSetting): String = {
    def enumClassToPsSetValue(enumClass: EnumClass): String = {
      enumClass.enumType match {
        case "Char" => "setString(idx, toSqlType(v).toString)"
        case _ => s"set${enumClass.enumType}(idx, toSqlType(v))"
      }
    }

    def enumClassToPsGetValue(enumClass: EnumClass): String = {
      enumClass.enumType match {
        case "Char" => "getString(idx).charAt(0)"
        case _ => s"get${enumClass.enumType}(idx)"
      }
    }

    def enumClassToPsUpdateValue(enumClass: EnumClass): String = {
      enumClass.enumType match {
        case "Char" => "updateString(idx, toSqlType(v).toString)"
        case _ => s"update${enumClass.enumType}(idx, toSqlType(v))"
      }
    }

    s"""trait ${enumClass.name}SqlTypeConverter extends ${setting.enumClassName}SqlTypeConverter[${enumClass.enumType}, ${enumClass.name}] {
       |  override def toSqlType(b: ${enumClass.name}): ${enumClass.enumType} = b.value
       |  override def fromSqlType(a: ${enumClass.enumType}): ${enumClass.name} = ${enumClass.name}.valueOf(a).fold(throw new ${setting.enumClassName}MappingNotFoundException("${enumClass.name} mapping not Found value=[a]", a))(identity)
       |}
       |
       |class ${enumClass.name}Mapper(val driver: JdbcProfile) {
       |  object TypeMapper extends driver.DriverJdbcType[${enumClass.name}] with ${enumClass.name}SqlTypeConverter {
       |    def sqlType = ${enumTypeToSqlType(enumClass.enumType)}
       |    override def setValue(v: ${enumClass.name}, p: PreparedStatement, idx: Int): Unit = p.${enumClassToPsSetValue(enumClass)}
       |    override def getValue(r: ResultSet, idx: Int): ${enumClass.name} = fromSqlType(r.${enumClassToPsGetValue(enumClass)})
       |    override def updateValue(v: ${enumClass.name}, r: ResultSet, idx: Int): Unit = r.${enumClassToPsUpdateValue(enumClass)}
       |    override def valueToSQLLiteral(value: ${enumClass.name}): ${enumClass.enumType} = driver.columnTypes.${enumClass.enumType.head.toString.toLowerCase + enumClass.enumType.tail}JdbcType.valueToSQLLiteral(value.value)
       |  }
       |
       |  object ${enumClass.name}GetResult extends ${setting.enumClassName}GetResult[${enumClass.enumType}, ${enumClass.name}] with ${enumClass.name}SqlTypeConverter {
       |    def next(rs: PositionedResult): ${enumClass.enumType} = rs.${enumTypeToNext(enumClass.enumType)}
       |    def nextOption(rs: PositionedResult): Option[${enumClass.enumType}] = rs.${enumTypeToNextOption(enumClass.enumType)}
       |  }
       |  object ${enumClass.name}SetParameter extends ${setting.enumClassName}SetParameter[${enumClass.enumType}, ${enumClass.name}] with ${enumClass.name}SqlTypeConverter {
       |    override def set(rs: PositionedParameters, d: ${enumClass.enumType}): Unit = rs.${enumTypeToSet(enumClass.enumType)}
       |    override def setOption(rs: PositionedParameters, d: Option[${enumClass.enumType}]): Unit = rs.${enumTypeToSetOption(enumClass.enumType)}
       |  }
       |}
       |""".stripMargin
  }

  protected def generateEnumClassMapperSupport(enumClasses: Seq[EnumClass])(setting: EnumClassSetting): String = {
    s"""
       |class ${setting.enumClassName}MapperSupport(val driver: JdbcProfile) {
       |${enumClasses.map(generateEnumClassDelegate).mkString("\n")}
       |
       |}
       |
       |object H2${setting.enumClassName}Support extends ${setting.enumClassName}MapperSupport(H2Driver)
       |object Postgres${setting.enumClassName}Support extends ${setting.enumClassName}MapperSupport(PostgresDriver)
       |object MySQL${setting.enumClassName}Support extends ${setting.enumClassName}MapperSupport(MySQLDriver)
       |object Hsqldb${setting.enumClassName}Support extends ${setting.enumClassName}MapperSupport(HsqldbDriver)
       |object SQLite${setting.enumClassName}Support extends ${setting.enumClassName}MapperSupport(SQLiteDriver)
       |
       |""".stripMargin
  }
  protected def generateEnumClassDelegate(enumClass: EnumClass): String = {
    s"""
       |  protected val ${enumClass.name}MapperDelegate = new ${enumClass.name}Mapper(driver)
       |
       |  implicit val ${enumClass.name}TypeMapper = ${enumClass.name}MapperDelegate.TypeMapper
       |  implicit val get${enumClass.name}Result = ${enumClass.name}MapperDelegate.${enumClass.name}GetResult.getResult
       |  implicit val get${enumClass.name}OptionResult = ${enumClass.name}MapperDelegate.${enumClass.name}GetResult.getOptionResult
       |  implicit val set${enumClass.name}Parameter = ${enumClass.name}MapperDelegate.${enumClass.name}SetParameter.setParameter
       |  implicit val set${enumClass.name}OptionParameter = ${enumClass.name}MapperDelegate.${enumClass.name}SetParameter.setOptionParameter
       |""".stripMargin
  }

  private def enumTypeToSet(enumType: String): String = {
    enumType match {
      case "Char" => "setString(d.toString)"
      case _ => s"set${enumType}(d)"
    }
  }

  private def enumTypeToSetOption(enumType: String): String = {
    enumType match {
      case "Char" => "setStringOption(d.map(_.toString))"
      case _ => s"set${enumType}Option(d)"
    }
  }


  private def enumTypeToNext(enumType: String): String = {
    enumType match {
      case "Char" => "nextString.charAt(0)"
      case _ => s"next${enumType}()"
    }
  }

  private def enumTypeToNextOption(enumType: String): String = {
    enumType match {
      case "Char" => "nextStringOption().map(_.charAt(0))"
      case _ => s"next${enumType}Option()"
    }
  }

  private def enumTypeToSqlType(enumType: String): String = {
    enumType match {
      case "String" => "java.sql.Types.VARCHAR"
      case "Int" => "java.sql.Types.INTEGER"
      case "Long" => "java.sql.Types.BIGINT"
      case "Char" => "java.sql.Types.CHAR"
      case _ => "java.sql.Types.VARCHAR"
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