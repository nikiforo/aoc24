val commonSettings = Seq(
  organization := "io.github.nikiforo",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.13.15"
)

val root = (project in file("."))
  .settings(
    name := "aoc24",
    commonSettings,
    addCompilerPlugin("com.olegpy" % "better-monadic-for_2.13" % "0.3.1"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
    )
  )
