import sbt.Keys.libraryDependencies

import Dependencies._


lazy val root = (project in file("."))
  .settings(
    name := "scala-dev-mooc-2021-11",
    version := "0.1",
    scalaVersion := "2.13.3",
    libraryDependencies ++= Dependencies.zio,
    libraryDependencies ++= Dependencies.pureconfig,
    libraryDependencies ++= Dependencies.zioConfig,
    libraryDependencies ++= Dependencies.http4sServer,
    libraryDependencies ++= Dependencies.circe,
    libraryDependencies ++= Dependencies.testContainers,
    libraryDependencies ++= Seq(
      "io.getquill"          %% "quill-jdbc-zio" % "3.12.0",
      "io.github.kitlangton" %% "zio-magic"      % "0.3.11",
      "org.postgresql"       %  "postgresql"     % "42.3.1"
    ),
    libraryDependencies ++= Seq(
      kindProjector,
      liquibase,
      logback
    ),

    addCompilerPlugin(Dependencies.kindProjector)
  )

 testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))

scalacOptions += "-Ymacro-annotations"

libraryDependencies += "org.scalatest"  %% "scalatest"    % "3.2.10" % "test"
libraryDependencies += "org.mockito"    % "mockito-core"  % "3.0.0" % "test"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.3"