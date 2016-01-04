package jp.co.bizreach.enumclassgen.setting

import jp.co.bizreach.enumclassgen.setting.EnumClassGeneratorSupport._

/**
  * Created by nishiyama on 2015/12/21.
  */
case class EnumClassSetting(
  enumSettingFile: String,
  sourcePath: String,
  packageName: String,
  enumClassName: String,
  documentPath: Option[String],
  support: Seq[EnumClassGeneratorSupport] = Nil
) {
  def supportSlick: Boolean = support.contains(SlickSupport)
  def supportPlay: Boolean = support.contains(PlaySupport)
}

sealed abstract class EnumClassGeneratorSupport

object EnumClassGeneratorSupport {
  case object SlickSupport extends EnumClassGeneratorSupport
  case object PlaySupport extends EnumClassGeneratorSupport
}
