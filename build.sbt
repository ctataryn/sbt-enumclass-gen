sbtPlugin := true

organization := "jp.co.bizreach"

name := "sbt-enumclass-gen"

version := "0.3.0-SNAPSHOT"

scalaVersion := "2.10.4"

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.yaml" % "snakeyaml" % "1.16",
  "com.typesafe" % "config" % "1.3.0"
)

publishTo := {
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at "s3://daemonby-sbt-repo/snapshots/")
  else
    Some("releases"  at "s3://daemonby-sbt-repo/releases/")
}