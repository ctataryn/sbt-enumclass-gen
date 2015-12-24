package jp.co.bizreach.enumclassgen.setting

/**
  * Created by nishiyama on 2015/12/21.
  */
case class EnumClassSetting(
  enumSettingFile: String,
  sourcePath: String,
  packageName: String,
  enumClassName: String,
  documentPath: Option[String],
  slickSupport: Boolean
)
