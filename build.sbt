organization := "com.santos.bruno"

scalaVersion := "2.11.0"

name := "fpwithscala"

version := "0.1-SNAPSHOT"

libraryDependencies ++=
    Seq("org.scalacheck" %% "scalacheck" % "1.12.5" % Test,
      "org.scalatest" %% "scalatest" % "2.2.4" % Test
    )


