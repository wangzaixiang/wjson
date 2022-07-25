import sbt.Keys.{publishConfiguration, publishTo}

ThisBuild / version := "0.1.0-RC2"

ThisBuild / scalaVersion := "3.1.3"

lazy val wjsonRoot = (project in file("."))
  .aggregate(wjson.js, wjson.jvm)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val wjson = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(
    name := "wjson",
    organization := "com.github.wangzaixiang",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test",
    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.1",

    publishMavenStyle := true,

    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (version.value.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },

    publishConfiguration  := publishConfiguration.value.withOverwrite(true),
    Test/publishArtifact := false,
    pomIncludeRepository := { _ => false },

    pomExtra := (
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

  )
  .jvmSettings(
    libraryDependencies += "org.mvel" % "mvel2" % "2.4.14.Final",
  )
  // .jvmConfigure( _.enablePlugins(GraalVMNativeImagePlugin) )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
  )