import aether.Aether._
import sbt.Keys._
import sbt._
import sbtrelease.ReleasePlugin._

object Build extends sbt.Build {

  val basename = "slick-crud"

  val scalaVersions = List("2.12.4", "2.11.12")

  override def settings = super.settings ++ Seq(
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xfuture",
      "-Ywarn-unused-import"
    ),
    organization       := "no.penger",
    scalaVersion       := scalaVersions.head,
    crossScalaVersions := scalaVersions
  )

  lazy val buildSettings = Defaults.coreDefaultSettings ++ aetherSettings ++ releaseSettings ++ Seq(
    updateOptions       := updateOptions.value.withCachedResolution(cachedResoluton = true),
    publishMavenStyle   := true,
    publish            <<= deploy,
    publishTo          <<= version { v ⇒
      val end = if(v endsWith "SNAPSHOT") "snapshot" else "release"
      Some("Finn-" + end at "http://mavenproxy.finntech.no/finntech-internal-" + end)
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

  val unfilteredVersion   = "0.9.1"

  val scalatest  = "org.scalatest" %% "scalatest" % "3.0.0" % "test"
  val testLogger = "org.slf4j" % "slf4j-simple" % "1.7.7"

  lazy val crud           = project("core")(
    "com.typesafe.slick"          %% "slick"                      % "3.0.3",
    scalatest,
    testLogger % "test"
  )

  lazy val crudUnfiltered = project("unfiltered", crud)(
    "ws.unfiltered"               %% "unfiltered-filter"          % unfilteredVersion,
    "no.arktekk"                  %% "linx"                       % "0.4",
    "javax.servlet"                % "javax.servlet-api"          % "3.1.0" % "provided;test"
  )

  lazy val crudLogging    = project("logging", crud)(
    "com.typesafe.scala-logging" %% "scala-logging"               % "3.5.0"
  )

  lazy val crudChangelog  = project("changelog", crud)(
    "joda-time" % "joda-time"    % "2.5",
    "org.joda"  % "joda-convert" % "1.7"
  )

  lazy val crudAll        = project("all", crudUnfiltered, crudChangelog, crudLogging)()

  lazy val crudDemo       = project("demo", crudAll)(
    "ws.unfiltered"               %% "unfiltered-jetty"           % unfilteredVersion,
    "com.h2database"               % "h2"                         % "1.4.186",
    scalatest,
    testLogger
  )

  lazy val root = Project(s"$basename-parent", file("."),
    settings = buildSettings :+ (mainClass in (Compile, run) := Some("no.penger.crud.Runner")))
    .aggregate(crud, crudLogging, crudChangelog, crudUnfiltered, crudAll, crudDemo).dependsOn(crudDemo)
}
