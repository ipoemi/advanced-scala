import Dependencies._

val catsVersion = "0.9.0"
val catsAll = "org.typelevel" %% "cats" % catsVersion withSources() withJavadoc()
val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "ipoemi",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "advanced-scala",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-language:_"
    ),
    libraryDependencies ++= Seq(
      catsAll, macroParadise, kindProjector, scalaCheck,
      scalaTest % Test
    )
  )
