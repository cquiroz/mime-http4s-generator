val Http4sVersion = "0.18.5"
val CirceVersion = "0.9.3"
val TreeHuggerVersion = "0.4.3"

lazy val root = (project in file("."))
  .settings(
    organization := "io.github.cquiroz",
    name := "mime-http4s-generator",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.12.5",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-client" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "io.circe"        %% "circe-generic"       % CirceVersion,
      "com.eed3si9n"    %% "treehugger"          % TreeHuggerVersion
    )
  )
