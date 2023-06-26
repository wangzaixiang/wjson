import sbt.Keys.{publishConfiguration, publishTo}

ThisBuild / version := "0.3.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

ThisBuild / organization := "com.github.wangzaixiang"

lazy val wjsonRoot = (project in file("."))
  .aggregate(core.jvm)
  .aggregate(schema)
  .settings(
    name := "wjsonRoot",
    publish / skip := true,
    publishLocal / skip := true,
    // publishLocal := {}
  )

// wjson core projects
lazy val core = crossProject(JVMPlatform).in(file("core"))
  .settings(
    name := "wjson-core",
    organization := "com.github.wangzaixiang",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
      "org.scala-lang" %% "scala3-library" % scalaVersion.value % "test",
    ),
    scalacOptions := Seq("-Yexplicit-nulls")
  )

lazy val debug = (project in file("debug"))
  .settings(
    name := "wjson-debug",
    organization := "com.github.wangzaixiang",
  )
  .dependsOn(core.jvm)

// JSON pattern matcher
lazy val matcher = project.in(file("matcher"))
  .settings(
    name := "wjson-matcher",
    organization := "com.github.wangzaixiang",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
    )
  )

// ADT support for JSON
lazy val schema = project.in(file("schema"))
  .settings(
    name := "wjson-schema",
    organization := "com.github.wangzaixiang",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
      "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
    )
  )
  .dependsOn(core.jvm)
//
//lazy val wjson = crossProject(JSPlatform, JVMPlatform).in(file("."))
//  .settings(
//    name := "wjson",
//    organization := "com.github.wangzaixiang",
//    libraryDependencies ++= Seq(
//      "org.scala-lang" %% "scala3-library" % scalaVersion.value,
//      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
//      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.1",
//    ),
//    scalacOptions := Seq("-deprecation", "-feature"),
//
//    publishMavenStyle := true,
//
//    publishTo := {
//      val nexus = "https://oss.sonatype.org/"
//      if (version.value.endsWith("SNAPSHOT"))
//        Some("snapshots" at nexus + "content/repositories/snapshots")
//      else
//        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
//    },
//
//    publishConfiguration  := publishConfiguration.value.withOverwrite(true),
//    Test/publishArtifact := false,
//    pomIncludeRepository := { _ => false },
//
//    pomExtra := (
//      <url>http://github.com/wangzaixiang/wjson</url>
//        <licenses>
//          <license>
//            <name>BSD-style</name>
//            <url>http://www.opensource.org/licenses/bsd-license.php</url>
//            <distribution>repo</distribution>
//          </license>
//        </licenses>
//        <developers>
//          <developer>
//            <id>wangzaixiang</id>
//            <name>wangzaixiang</name>
//            <url>http://wangzaixiang.github.io</url>
//          </developer>
//        </developers>
//        <scm>
//          <connection>scm:git:https://github.com/wangzaixiang/wjson</connection>
//          <developerConnection>scm:git:https://github.com/wangzaixiang/wjson</developerConnection>
//          <url>github.com/wangzaixiang/wjson</url>
//          <tag>v0.1.0</tag>
//        </scm>
//      )
//
//  )
//  .jvmSettings(
//    libraryDependencies += "org.mvel" % "mvel2" % "2.4.14.Final",
//  )
////   .jvmConfigure( _.enablePlugins(GraalVMNativeImagePlugin) )
////   .jvmConfigure( _.enablePlugins(ScalaNativePlugin) )
//  .jsSettings(
//    scalaJSUseMainModuleInitializer := true,
//  )

//lazy val schema = project.in(file("schema"))
//  .settings(
//    name := "wjson-schema",
//    libraryDependencies ++= Seq(
//      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
//      "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
//      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value
//    ),
//  )
//  .dependsOn(wjson.jvm)