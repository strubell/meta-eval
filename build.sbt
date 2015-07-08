import com.typesafe.sbt.SbtNativePackager.packageArchetype

import org.allenai.plugins.CoreDependencies

import org.allenai.plugins.archetypes.LibraryPlugin

import org.allenai.plugins.CoreRepositories.PublishTo

name := "meta-eval"

organization := "org.allenai.scholar.metrics.metadata"

packageArchetype.java_application

scalaVersion := "2.11.5"

resolvers += "IESL Snapshots" at "https://dev-iesl.cs.umass.edu/nexus/content/repositories/snapshots"

libraryDependencies ++= Seq(
  CoreDependencies.allenAiCommon,
  CoreDependencies.allenAiTestkit % "test",
  "org.apache.commons" % "commons-lang3" % "3.0")

libraryDependencies += "org.jsoup" % "jsoup" % "1.8.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

// for iesl eval stuff
libraryDependencies += "cc.factorie" % "factorie_2.11" % "1.2-SNAPSHOT"

dependencyOverrides ++= Set(
  "com.typesafe" % "config" % "1.2.1"
)

conflictManager := ConflictManager.latestRevision

PublishTo.ai2Public

enablePlugins(LibraryPlugin)