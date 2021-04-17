name := "minesweeper-ws"

version := "0.1"

scalaVersion := "2.13.5"

scalacOptions ++= Seq("-Xlint:strict-unsealed-patmat", "-Xfatal-warnings", "-deprecation")
scalacOptions in Global += "-Ymacro-annotations"
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)

val tapirVersion = "0.18.0-M4"
val monocleVersion = "3.0.0-M4"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "httpclient-backend-zio" % "3.2.3",
  "dev.zio" %% "zio-logging" % "0.5.8",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.2",

  "com.softwaremill.sttp.tapir" %% "tapir-zio" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-zio-http4s-server" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-refined" % tapirVersion,

  "dev.zio" %% "zio-interop-cats" % "2.4.0.0",

  "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion
)
