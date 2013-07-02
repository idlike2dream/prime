name := "prime"

version := "0.0.0"

scalaVersion := "2.11.0-M3"

scalacOptions ++= Seq("-unchecked",
                      "-feature",
                      "-Xlint")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
