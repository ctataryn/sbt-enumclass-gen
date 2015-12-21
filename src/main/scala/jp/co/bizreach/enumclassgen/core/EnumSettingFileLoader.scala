package jp.co.bizreach.enumclassgen.core

import java.io.{FileInputStream, BufferedInputStream, File}
import java.util
import scala.collection.JavaConverters._

import org.yaml.snakeyaml.Yaml

/**
  * Created by nishiyama on 2015/12/17.
  */
object EnumSettingFileLoader extends FileUtils {

  def loadEnumClassYaml(file: File): Seq[EnumClass] = {

    val enumMappingRaws = using(new BufferedInputStream(new FileInputStream(file))) { is =>
      val yaml = new Yaml()
      yaml.load(is).asInstanceOf[util.List[util.Map[String, Any]]].asScala.filter(_ != null)
    }

    val enumClasses = enumMappingRaws.flatMap { enumMappingRaw =>
      val enumMapping = enumMappingRaw.asScala
      for {
        enumName <- enumMapping.get("name").map(_.toString)
      } yield {
        val description = enumMapping.get("description").map(_.toString)
        val enumType = enumMapping.get("type").map(_.toString).getOrElse("String")
        val enumApplyType = enumMapping.get("applyType").map(_.toString)

        val attrKeys = enumMapping.get("attrKeys").map(_.asInstanceOf[util.List[util.Map[String, Any]]].asScala).map { attrKeysRaws =>
          attrKeysRaws.flatMap { attrKeyMapRaw =>
            val attrKeyMap = attrKeyMapRaw.asScala
            attrKeyMap.get("name").map(_.toString).map { attrName =>
              val attrType = attrKeyMap.get("type").map(_.toString).getOrElse("String")
              val required = attrKeyMap.get("required").map(_.toString).exists {
                case "true" => true
                case "false" => false
              }
              EnumAttrKey(attrName, attrType, required)
            }
          }
        }.getOrElse(Nil)

        val values = enumMapping.get("values").map(_.asInstanceOf[util.List[util.Map[String, Any]]].asScala).getOrElse(Nil).flatMap { valueRawMap =>
          val valueMap = valueRawMap.asScala
          for {
            valueName <- valueMap.get("name").map(_.toString)
            value <- valueMap.get("value").map(_.toString)
          } yield {
            val comment = valueMap.get("comment").map(_.toString)
            val applyValue = valueMap.get("applyValue").map(_.toString)
            val attrs = valueMap.get("attrs").map(_.asInstanceOf[util.Map[String, String]].asScala).map { attrMap =>
              if (!attrKeys.filter(_.required).forall(key => attrMap.contains(key.name)))
                throw new IllegalStateException("Required Attr key not contains")

              attrKeys.flatMap{ key =>
                attrMap.get(key.name).map{ value =>
                  EnumAttrValue(key, value)
                }
              }
            }.getOrElse(Nil)
            EnumValue(valueName, value, comment, applyValue, attrs)
          }
        }
        EnumClass(enumName, description, enumType, enumApplyType, attrKeys, values)
      }
    }

    enumClasses

  }

}
