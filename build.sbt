name := "Typesetter"

version := "0.4"

scalaVersion := "2.11.6"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

incOptions := incOptions.value.withNameHashing( true )

organization := "ca.hyperreal"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"

import AssemblyKeys._

assemblySettings

mainClass in assembly := Some( "ca.hyperreal.sscheme.Main" )

jarName in assembly := name.value + "-" + version.value + ".jar"


seq(bintraySettings:_*)


publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

homepage := Some(url("https://github.com/edadma/typesetter"))

pomExtra := (
  <scm>
    <url>git@github.com:edadma/typesetter.git</url>
    <connection>scm:git:git@github.com:edadma/typesetter.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>http://lteditor.org</url>
    </developer>
  </developers>)
