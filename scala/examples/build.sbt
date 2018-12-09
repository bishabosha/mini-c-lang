val dottyVersion = "0.11.0-RC1"
// val dottyVersion = dottyLatestNightlyBuild.get
val graalvmVersion = "1.0.0-rc9"

lazy val root = project
  .in(file("."))
  .settings(
    name := "examples",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    // https://mvnrepository.com/artifact/org.graalvm/graal-sdk
    libraryDependencies += "org.graalvm.sdk" % "graal-sdk" % graalvmVersion,

    libraryDependencies += "com.oracle.substratevm" % "svm" % graalvmVersion,

    libraryDependencies += "org.graalvm.truffle" % "truffle-api" % graalvmVersion,

    libraryDependencies += "org.graalvm.compiler" % "compiler" % graalvmVersion
  )