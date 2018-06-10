name := "spotify-track"

version := "0.1"

scalaVersion := "2.12.6"
scalacOptions += "-Ypartial-unification" // 2.11.9+
mainClass in Compile := Some("net.hhamalai.SpotifyConnector")


projectDependencies ++= List(
  "com.softwaremill.sttp" %% "core" % "1.1.14",
  "com.softwaremill.sttp" %% "circe" % "1.1.14",
  "io.circe" %% "circe-generic" % "0.9.3",
  "org.tpolecat" %% "doobie-core"      % "0.5.3",
  "org.xerial" % "sqlite-jdbc" % "3.23.1",
  "joda-time" % "joda-time" % "2.9.9",
  "com.lihaoyi" %% "scalatags" % "0.6.7"
)