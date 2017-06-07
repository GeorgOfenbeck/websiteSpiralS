name := """websiteSpiralS"""

version := "2.5.x"

scalaVersion := "2.11.11"

libraryDependencies += filters

libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.0" % "test"


libraryDependencies += "com.typesafe.play" %% "play-slick" % "2.0.2"
libraryDependencies += "com.typesafe.play" %% "play-slick-evolutions" % "2.0.2"
libraryDependencies += "com.h2database" % "h2" % "1.4.194"

libraryDependencies += specs2 % Test
libraryDependencies += filters

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-library" % _ % "compile")

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "compile")

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"

libraryDependencies ++= Seq(
  //"org.scalatest" %% "scalatest" % "2.2.5-M3" % "test"
  "org.scalatest" %% "scalatest" % "3.0.0" % "test" // cross CrossVersion.full"
)

libraryDependencies += "com.twitter" %% "chill-akka" % "0.8.0"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" // cross CrossVersion.full"

libraryDependencies += "org.scala-lang.modules" %% "spores-core" % "0.2.4"

libraryDependencies += "org.scala-lang.modules" %% "spores-pickling" % "0.2.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2"

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.7"

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-io-extra" % "2.1.7"

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-filters" % "2.1.7"

libraryDependencies += "org.tinyjee.jgraphx" % "jgraphx" % "3.4.1.3"

libraryDependencies += "ch.epfl.lara" % "scife_2.11" % "1.2.9"

libraryDependencies += "com.github.benhutchison" %% "scalaswingcontrib" % "1.7"

libraryDependencies += "com.github.wookietreiber" %% "scala-chart" % "latest.integration"

libraryDependencies += "com.itextpdf" % "itextpdf" % "5.5.6"

libraryDependencies += "org.jfree" % "jfreesvg" % "3.0"

libraryDependencies += "com.github.wendykierp" % "JTransforms" % "3.1"


lazy val root = (project in file(".")).enablePlugins(PlayScala)

