name := "learnScala"

organization := "misoul"

version := "0.1"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq("joda-time" % "joda-time" % "1.6",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test",
  "org.specs2" %% "specs2" % "2.3.12" % "test",
  "junit" % "junit" % "4.7" % "test"
)
