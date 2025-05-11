// See README.md for license details.

name := "gemmini"

version := "3.1.0"

// scalaVersion := "2.13.10" // original
scalaVersion := "2.12.15"

// libraryDependencies ++= Seq(
//   // "edu.berkeley.cs" %% "chisel3" % "3.6.0", // original
//   "edu.berkeley.cs" %% "chisel3" % "3.5.6",
//   // "edu.berkeley.cs" %% "rocketchip" % "1.2.+", // original
//   "org.scalanlp" %% "breeze" % "1.1")

// libraryDependencies += "edu.berkeley.cs" %% "chisel-iotesters" % "1.5-SNAPSHOT"
// libraryDependencies += "edu.berkeley.cs" %% "firrtl" % "1.4.0" // for the iotesters.Driver.execute(Array("--backend-name", "treadle",...)
// dependencyOverrides += "edu.berkeley.cs" %% "firrtl" % "1.4.0"

// FORCE FIRRTL 1.4.0
// dependencyOverrides += "edu.berkeley.cs" %% "firrtl" % "1.5.6"
dependencyOverrides += "edu.berkeley.cs" %% "firrtl-interpreter" % "1.5.6"
dependencyOverrides += "edu.berkeley.cs" %% "chisel3" % "3.5.6"
dependencyOverrides += "edu.berkeley.cs" %% "chisel-iotesters" % "1.5-SNAPSHOT"

libraryDependencies += "edu.berkeley.cs" %% "firrtl-interpreter" % "1.5.6" % Test // add Firrtl as a test dependency explicitly
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.5.6"
libraryDependencies += "edu.berkeley.cs" %% "chisel-iotesters" % "1.5-SNAPSHOT"


// Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat


// Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat // does not solve issue
// Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary // does not solve issue



resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.mavenLocal)

// specified commit BEFORE scala bump to 2.13 for compatibility
// need this version for MulRecFN and fast divider
lazy val newHardfloat = RootProject(uri("https://github.com/ucb-bar/berkeley-hardfloat.git#74cc28")) // this was originally commented out

lazy val root = (project in file(".")) // I removed all firesim dependency stuff like midas_target_utils
  .dependsOn(newHardfloat)
  .aggregate(newHardfloat)