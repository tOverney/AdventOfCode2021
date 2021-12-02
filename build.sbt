ThisBuild / scalaVersion := "3.1.0"
ThisBuild / organization := "ch.overney"

val adventOfCode =
  project
    .in(file("."))
    .settings(name := "Advent Of Code")
