val dottyVersion = "0.10.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    // https://mvnrepository.com/artifact/org.graalvm/graal-sdk
    libraryDependencies += "org.graalvm" % "graal-sdk" % "1.0.0-rc7"
  )
