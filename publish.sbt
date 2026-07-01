ThisBuild / organization := "co.computist"
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/garlandus/augment"),
    "scm:git@github.com:garlandus/augment.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "garlandus",
    name = "garlandus",
    email = "garlandcomputist@gmail.com",
    url = url("https://github.com/garlandus")
  )
)
ThisBuild / description := "Augmented functions"
ThisBuild / licenses := List(
  "EUPL-1.2" -> new URL("https://eupl.eu/1.2/en")
)
ThisBuild / homepage := Some(url("http://computist.co/augment.html"))

ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true

