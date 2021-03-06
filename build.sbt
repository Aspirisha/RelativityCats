lazy val scalaV = "2.11.8"
lazy val playVersion = "2.5.9"

lazy val server = (project in file("server")).settings(
  scalaVersion := scalaV,
  scalaJSProjects := Seq(client),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  pipelineStages := Seq(digest, gzip),
  // triggers scalaJSPipeline when using compile or continuous compilation
  compile in Compile <<= (compile in Compile) dependsOn scalaJSPipeline,
  libraryDependencies ++= Seq(
    "com.vmunier" %% "scalajs-scripts" % "1.0.0",
    specs2 % Test,
    "com.typesafe.akka" % "akka-persistence_2.11" % "2.4.14",
    "org.iq80.leveldb"            % "leveldb"          % "0.7",
    "org.fusesource.leveldbjni"   % "leveldbjni-all"   % "1.8",
    "com.typesafe.akka" %% "akka-cluster-tools" % "2.4.14"
  ),
  // Compile the project before generating Eclipse files, so that generated .scala or .class files for views and routes are present
  EclipseKeys.preTasks := Seq(compile in Compile)
).enablePlugins(PlayScala).
  dependsOn(sharedJvm)

lazy val client = (project in file("client")).settings(
  scalaVersion := scalaV,
  persistLauncher := true,
  persistLauncher in Test := false,
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.1",
    "org.akka-js" %%% "akkajsactor" % "0.2.4.16",
    "com.lihaoyi" %%% "scalatags" % "0.6.2",
    "com.typesafe.akka" %% "akka-http-core" % "10.0.0",
    "be.doeraene" %%% "scalajs-pickling" % "0.4.0",
    "com.typesafe.akka" %% "akka-http" % "10.0.0")
).enablePlugins(ScalaJSPlugin, ScalaJSWeb).
  dependsOn(sharedJs)

lazy val shared = (crossProject.crossType(CrossType.Pure) in file("shared")).
  settings(scalaVersion := scalaV,
    libraryDependencies ++= Seq("com.typesafe.play" %% "play" % playVersion,
                                "com.outr" %%% "scribe" % "1.2.6",
                                "com.uniformlyrandom" %%% "jello" % "0.3.0")).
  jsConfigure(_ enablePlugins ScalaJSWeb)

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

// loads the server project at sbt startup
onLoad in Global := (Command.process("project server", _: State)) compose (onLoad in Global).value
