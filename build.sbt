name := "scala-cats"

version := "0.1"

scalaVersion := "3.3.6"

val catsVersion = "2.13.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)
