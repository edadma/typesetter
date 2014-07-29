name := "Typesetter"

version := "0.1"

scalaVersion := "2.11.1"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "local"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"
