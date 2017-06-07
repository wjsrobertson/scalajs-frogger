enablePlugins(ScalaJSPlugin)

name := "frogger"
scalaVersion := "2.11.8"

scalaJSUseMainModuleInitializer := true
relativeSourceMaps := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
libraryDependencies += "com.github.benhutchison" %%% "gesture" % "0.3"

jsDependencies += RuntimeDOM

libraryDependencies += "junit" % "junit" % "4.12" % "test"
libraryDependencies += "org.mockito" % "mockito-all" % "1.10.19" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

logBuffered in Test := false