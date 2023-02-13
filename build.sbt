ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

val catsVersion = "2.7.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
)


lazy val root = (project in file("."))
  .settings(
    name := "scala3-bootcamp2"
  )
