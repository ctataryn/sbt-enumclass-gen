package jp.co.bizreach.enumclassgen.core

/**
  * Created by nishiyama on 2015/12/17.
  */
case class EnumAttrKey(name: String, attrType: String, required: Boolean = false)
case class EnumAttrValue(key: EnumAttrKey, value: String)
case class EnumValue(name: String, value: String, comment: Option[String], applyValue: Option[String] = None, attrs: Seq[EnumAttrValue] = Nil)
case class EnumClass(name: String, description: Option[String], enumType: String, applyType: Option[String] = None, attrKeys: Seq[EnumAttrKey] = Nil, values: Seq[EnumValue] = Nil)
