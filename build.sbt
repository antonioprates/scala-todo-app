name := "TodoBook"

version := "1.0"

scalaVersion := "2.11.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.5.22"
libraryDependencies += "com.typesafe.akka" %% "akka-persistence" % "2.5.22"
