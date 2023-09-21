import sbt.Keys.{publishConfiguration, publishTo}

ThisBuild / organization := "com.github.wangzaixiang"
ThisBuild / version := "0.3.0-RC1"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / versionScheme := Some("early-semver")

ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / pomExtra := (
  <url>http://github.com/wangzaixiang/wjson</url>
    <licenses>
      <license>
        <name>BSD-style</name>
        <url>http://www.opensource.org/licenses/bsd-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <developers>
      <developer>
        <id>wangzaixiang</id>
        <name>wangzaixiang</name>
        <url>http://wangzaixiang.github.io</url>
      </developer>
    </developers>
    <scm>
      <connection>scm:git:https://github.com/wangzaixiang/wjson</connection>
      <developerConnection>scm:git:https://github.com/wangzaixiang/wjson</developerConnection>
      <url>github.com/wangzaixiang/wjson</url>
      <tag>v0.1.0</tag>
    </scm>
  )


lazy val wjsonRoot = (project in file("."))
  .aggregate(core)
  .aggregate(schema)
  .settings(
    name := "wjson",
    publish / skip := true,
    publishLocal / skip := true,
  )

// wjson core projects
lazy val core = (project in file("core"))
  .settings(
    name := "wjson-core",
    organization := "com.github.wangzaixiang",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
      "org.scala-lang" %% "scala3-library" % scalaVersion.value % "test",
    ),
    scalacOptions := Seq("-Yexplicit-nulls")
  )

// JSON pattern matcher
lazy val pattern = project.in(file("pattern"))
  .dependsOn(core)
  .settings(
    name := "wjson-pattern",
    organization := "com.github.wangzaixiang",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0",
      "org.mvel" % "mvel2" % "2.4.11.Final",
    )
  )

// ADT support for JSON
lazy val schema = project.in(file("schema"))
  .dependsOn(core)
  .settings(
    name := "wjson-schema",
    organization := "com.github.wangzaixiang",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
      "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
    )
  )