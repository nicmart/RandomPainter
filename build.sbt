
enablePlugins(ScalaJSPlugin, WorkbenchPlugin)

name := "Paint"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.0"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  "com.lihaoyi" %%% "scalatags" % "0.6.2",
  "org.typelevel" %% "cats" % "0.8.1",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
