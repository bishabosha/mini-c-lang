val dottyVersion = "0.19.0-RC1"
// val dottyVersion = dottyLatestNightlyBuild.get
val graalvmVersion = "19.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "mmc",
    version := "0.1.0",

    scalacOptions ++= Seq("-noindent"),

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    // https://mvnrepository.com/artifact/org.graalvm/graal-sdk
    libraryDependencies += "org.graalvm.sdk" % "graal-sdk" % graalvmVersion,

    libraryDependencies += "com.oracle.substratevm" % "svm" % graalvmVersion,

    libraryDependencies += "org.graalvm.truffle" % "truffle-api" % graalvmVersion,

    libraryDependencies += "org.graalvm.compiler" % "compiler" % graalvmVersion

  )
