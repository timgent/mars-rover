lazy val root = (project in file("."))
  .settings(
    organization in ThisBuild := "com.example",
    scalaVersion in ThisBuild := "2.12.10",
    version in ThisBuild := "0.1.0-SNAPSHOT",
    name := "Hello",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % Test,
      "com.beachape" %% "enumeratum" % "1.7.0"
    )
  )
