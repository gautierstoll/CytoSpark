name := "CytoSpark"

version := "0.1"

scalaVersion := "2.11.8"
//scalaVersion := "2.12.4"

crossScalaVersions := Seq("2.11.8","2.12.4")

//libraryDependencies ++= Seq(
  //"org.apache.spark" % "spark-core_2.10" % "2.0.0",
  //"org.apache.spark" % "spark-sql_2.10" % "2.0.0"
  //)

libraryDependencies ++= Seq("org.apache.spark" %% "spark-sql" % "2.3.0",
    "org.apache.spark" %% "spark-mllib" % "2.3.0")

libraryDependencies  ++= Seq(
  // Last stable release
  "org.scalanlp" %% "breeze" % "0.13.2",

  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.13.2",

  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "0.13.2"
)

libraryDependencies += "io.github.pityka" %% "nspl-awt" % "0.0.19"
libraryDependencies +=
  "io.github.pityka" %% "saddle-core-fork" % "1.3.4-fork1" exclude ("com.googlecode.efficient-java-matrix-library", "ejml")

libraryDependencies += "io.github.pityka" %% "nspl-saddle" % "0.0.19"
libraryDependencies += "org.ejml" % "core" % "0.27"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"