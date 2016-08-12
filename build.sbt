name := "learnScala"

organization := "misoul"

version := "0.1"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.15",
  "com.typesafe.akka" %% "akka-slf4j" % "2.3.15",
  "org.apache.spark" %% "spark-core" % "2.0.0",

  "joda-time" % "joda-time" % "1.6",
  "org.scalatest" %% "scalatest" % "2.2.0" % "test",
  "org.specs2" %% "specs2" % "2.3.12" % "test",
  "junit" % "junit" % "4.7" % "test"
)

resolvers ++= Seq(
    "Spray repository" at "http://repo.spray.io",
    "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)

// object Projects {
//   lazy val depProject = RootProject(uri("git://github.com/aboisvert/skiis.git#%s".format(V.depProject)))
// }

// set the main class for 'sbt run'
mainClass in (Compile, run) := Some("com.interviews.radius.TopReachableLeads")
