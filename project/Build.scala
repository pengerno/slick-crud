import aether.Aether._
import sbt.Keys._
import sbt._
import sbtrelease.ReleasePlugin._

object Build extends sbt.Build {

  val basename = "slick-crud"
  val finnRepo = "Finn repo" at "http://mavenproxy.finntech.no/finntech-internal-release"

  override def settings = super.settings ++ Seq(
    scalacOptions      := Seq("-unchecked", "-deprecation", "-encoding", "UTF-8", "-feature"),
    organization       := "no.penger",
    scalaVersion       := "2.11.4",
    resolvers         ++= Seq(finnRepo)
  )


  lazy val buildSettings = Defaults.coreDefaultSettings ++ aetherSettings ++ releaseSettings ++ Seq(
    publishMavenStyle  := true,
    publish <<= deploy,
    publishTo <<= version { v ⇒
      val proxy = "http://mavenproxy.finntech.no/finntech-internal-"
      val end = if(v endsWith "SNAPSHOT") "snapshot" else "release"
      Some("Finn-" + end at proxy + end)
    }
  )

  val scalaReflect = "org.scala-lang" % "scala-reflect"

  def project(suffix: String, projectDeps: sbt.ClasspathDep[sbt.ProjectReference]*)(deps: ModuleID*) =
    Project(
      id           = s"$basename-$suffix",
      base         = file(s"./$suffix"),
      dependencies = projectDeps,
      settings     = buildSettings ++ Seq(libraryDependencies ++= deps :+ scalaReflect % scalaVersion.value)
    )

  val transactionsVersion = "2"
  val unfilteredVersion   = "0.8.2"

  lazy val crud = project("core")(
    "com.typesafe.slick"          %% "slick"                      % "2.1.0",
    "org.scalatest"               %% "scalatest"                  % "2.1.7" % "test"
  )

  lazy val crudUnfiltered = project("unfiltered", crud)(
    "net.databinder"              %% "unfiltered-filter"          % unfilteredVersion,
    "javax.servlet"                % "javax.servlet-api"          % "3.1.0" % "provided;test"
  )

  lazy val crudLogging = project("logging", crud)(
    "com.typesafe.scala-logging" %% "scala-logging"               % "3.1.0"
  )

  lazy val crudDemo = project("demo", crudLogging, crudUnfiltered)(
    "no.penger"                   %% "tx-testing-liquibase"       % transactionsVersion,
    "net.databinder"              %% "unfiltered-jetty"           % unfilteredVersion,
    "org.slf4j"                    % "slf4j-simple"               % "1.7.7",

    "no.penger"                   %% "tx-testing-liquibase"       % transactionsVersion % "test",
    "org.scalatest"               %% "scalatest"                  % "2.2.2" % "test"
  )

  lazy val root = Project(s"$basename-parent", file("."),
    settings = buildSettings :+ (mainClass in (Compile, run) := Some("no.penger.crud.Runner")))
    .aggregate(crud, crudLogging, crudUnfiltered, crudDemo).dependsOn(crudDemo)
}
