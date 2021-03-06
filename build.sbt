name := "TodoBook"

version := "1.0"

// TODO: update to 2.12.10
scalaVersion := "2.11.1"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.5.22"
libraryDependencies += "com.typesafe.akka" %% "akka-persistence" % "2.5.22"

libraryDependencies += "org.iq80.leveldb" % "leveldb" % "0.7"
libraryDependencies += "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
