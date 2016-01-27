package jp.co.bizreach.enumclassgen.generator

import java.io.File

import jp.co.bizreach.enumclassgen.core.{EnumClass, EnumSettingFileLoader}
import jp.co.bizreach.enumclassgen.setting.EnumClassSetting
import sbt.IO

/**
  * Created by nishiyama on 2016/01/04.
  */
trait EnumSlickSupportGenerator {
  def generateEnumSlickSupportFile(setting: EnumClassSetting): Unit = {
    if (setting.supportSlick) {
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
        |class ${setting.enumClassName}MappingNotFoundException[A](message: String, value: A) extends Exception(message) {
        |  override def getMessage: String = {
        |    message + s"value=$$value"
        |  }
        |}
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
        |  override def fromSqlType(a: ${enumClass.enumType}): ${enumClass.name} = ${enumClass.name}.valueOf(a).fold(throw new ${setting.enumClassName}MappingNotFoundException("${enumClass.name} mapping not Found", a))(identity)
        |}
        |
       |class ${enumClass.name}Mapper(val driver: JdbcProfile) {
        |  object TypeMapper extends driver.DriverJdbcType[${enumClass.name}] with ${enumClass.name}SqlTypeConverter {
        |    def sqlType = ${enumTypeToSqlType(enumClass.enumType)}
        |    override def setValue(v: ${enumClass.name}, p: PreparedStatement, idx: Int): Unit = p.${enumClassToPsSetValue(enumClass)}
        |    override def getValue(r: ResultSet, idx: Int): ${enumClass.name} = fromSqlType(r.${enumClassToPsGetValue(enumClass)})
        |    override def updateValue(v: ${enumClass.name}, r: ResultSet, idx: Int): Unit = r.${enumClassToPsUpdateValue(enumClass)}
        |    override def valueToSQLLiteral(value: ${enumClass.name}): String = driver.columnTypes.${enumClass.enumType.head.toString.toLowerCase + enumClass.enumType.tail}JdbcType.valueToSQLLiteral(value.value)
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

