resolvers ++= Seq(
  "Maven Central Server" at "https://repo1.maven.org/maven2",
  "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

scalacOptions := Seq(
  "-deprecation",
  "-encoding",
  "utf-8",
  "-feature",
  "-explain-types",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "check-init",
  "-indent",
  "-rewrite"
)

ThisBuild / organization := "io.typechecked"
ThisBuild / organizationName := "alphabet-soup"
ThisBuild / scalaVersion := "3.3.1"

ThisBuild / publishTo          := Some(
  "Artifactory Realm" at "https://artifactory.orgvue.com:443/artifactory/company-sbt-release"
)

lazy val root = (project in file("."))
  .settings(
    name := "alphabet-soup",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.typelevel" %% "shapeless3-deriving" % "3.3.0",
      "org.scalatest" %% "scalatest" % "3.2.17" % "test"
    )
  )