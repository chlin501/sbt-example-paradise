import sbt._
import Keys._

object BuildSettings {
  val paradiseVersion = "2.1.0-M5"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "none",
    version := "1.0.0",
    scalacOptions ++= Seq("-deprecation"),
    scalaVersion := "2.11.6",
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.10.5", "2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4", "2.11.5", "2.11.6"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      scalacOptions := Seq("-deprecation"),
      run <<= run in Compile in core
    )
  ) aggregate(macros, core)

  lazy val macros: Project = Project(
    "grapher",
    file("grapher"),
    settings = buildSettings ++ Seq(
      scalacOptions := Seq("-deprecation"),
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= (
        if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
        else Nil
      )
    )
  )

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings ++ Seq(libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.12", scalacOptions := Seq("-deprecation"))
  ) dependsOn(macros)
}
