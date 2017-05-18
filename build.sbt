enablePlugins(ScalaJSPlugin)

name := "frogger"
scalaVersion := "2.11.8"

scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

jsDependencies += RuntimeDOM
