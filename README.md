# SBT EnumClass Gen
 
that will generate enumeration case classes.

When use case object instead of scala enumeration, many boilerplate codes. 
This plugin generate case objects code and markdown document from enum-mapping.yml files.


## Example 

* enum-mapping.yml

```
- name: MemberStatus
  description: status for member
  type: String
  attrKeys:
    - {name: "aliasJa", type: "String", required: true}
  values:
    - name: Provisional
      value: PRV
      comment: Sign up status but not authorized
      attrs:
        aliasJa: "仮会員"
    - name: Formalized
      value: FML
      # comment is optional
      attrs:
        aliasJa: 正会員
    - name: Withdrawal
      value: WDL
      attrs:
        aliasJa: 退会会員

- name: Flg
  description: Flg for databases
  type: Char
  applyType: Boolean # Optional generate apply method for another type
  values:
    - name: "True"
      value: Y
      applyValue: true
    - name: "False"
      value: Y
      applyValue: false

```

* case object class

```
package models

/** Base trait for sealed class */
trait SealedEnum[+A] {
  val value: A
}

trait SealedEnumCompanion[B <: SealedEnum[A], A] {
  val values: Seq[B]
  val valueMap: Map[A, B]
  def valueOf(value: A): Option[B] = valueMap.get(value)
}

/** MemberStatus: status for member */
abstract sealed class MemberStatus(val value: String, val aliasJa: String) extends SealedEnum[String]
object MemberStatus extends SealedEnumCompanion[MemberStatus, String] {
  /** Provisional: Sign up status but not authorized */
  case object Provisional extends MemberStatus("PRV", aliasJa = "仮会員")
  /** Formalized:  */
  case object Formalized extends MemberStatus("FML", aliasJa = "正式会員")
  /** Withdrawal:  */
  case object Withdrawal extends MemberStatus("WDL", aliasJa = "正式会員")

  val values = Seq(Provisional, Formalized, Withdrawal)
  val valueMap = Map("PRV" -> Provisional, "FML" -> Formalized, "WDL" -> Withdrawal)

}

/** Flg: Flg for databases */
abstract sealed class Flg(val value: Char, val tpe: Boolean) extends SealedEnum[Char]
object Flg extends SealedEnumCompanion[Flg, Char] {
  /** True:  */
  case object True extends Flg('Y', tpe = true)
  /** False:  */
  case object False extends Flg('Y', tpe = false)

  val values = Seq(True, False)
  val valueMap = Map('Y' -> True, 'Y' -> False)
  val applyMap: Map[Boolean, Flg] = Map(true -> True, false -> False)
  def apply(tpe: Boolean): Flg = applyMap.get(tpe).get

  Flg(true) == Flg.True
}
```

* and document markdown

see example/docs/markdown.md
 
## Using the plugin

1. add following to your project's `plugin.sbt` 

`addSbtPlugin("jp.co.bizreach" % "sbt-enumclass-gen" % "0.3.0-SNAPSHOT")`

2. create mapping.yml

see below file format section

3. add setting to your project's `build.sbt`

```
enumClassSettings := Seq(
  EnumClassSetting(
    enumSettingFile = "enumclass.yml",
    sourcePath = "app",
    packageName = "models",
    enumClassName = "Enums",
    documentPath = Some("docs/enums.md")
  )
)
```

4. run command `sbt generateEnumClass`

## yml file format

```
## class name
- name: MemberStatus
  ## class comment (optional)
  description: status for member
  ## value type (String|Char|Integer|Long)
  type: String
  ## optional attributes
  attrKeys:
    ## you can use json type format in yaml format
    - {name: "aliasJa", type: "String", required: true}
      ## property name
    - name: orderNo
      ## property type (String|Char|Integer|Long)
      type: Integer
      ## if not required then property type is Option[<type>]
      required: true
  ## case object values
  values:
    ## case object name
    - name: Provisional
      ## actual value
      value: PRV
      ## value comment (optional)
      comment: Sign up status but not authorized
      ## if add attrKeys required attrs key
      attrs:
        ## required key
        aliasJa: "仮会員"
        ## not required key is optional
    - name: Formalized
      value: FML
      # comment is optional
      attrs:
        aliasJa: 正会員
    - name: Withdrawal
      value: WDL
      attrs:
        aliasJa: 退会会員
```

## build.sbt configuration

- enumClassSettings
 - `Seq(EnumClassSetting)`
 - default: Nil

### EnumClassSetting property

- enumSettingFile
 - yaml file
- sourcePath
 - play2-framework -> `app`
- packageName
 - geneate class package
- enumClassName
 - generate scala file name
- documentPath(optional)
 - markdown document file path
