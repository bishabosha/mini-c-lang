val scala3 = "3.3.1-RC1-bin-SNAPSHOT"
val graalvmVersion = "22.3.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "mmc",
    version := "0.1.0",

    scalacOptions ++= Seq("-deprecation"),

    scalaVersion := scala3,

    // libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    // https://mvnrepository.com/artifact/org.graalvm/graal-sdk
    libraryDependencies += "org.graalvm.sdk" % "graal-sdk" % graalvmVersion,

    // libraryDependencies += "com.oracle.substratevm" % "svm" % graalvmVersion,

    libraryDependencies += "org.graalvm.truffle" % "truffle-api" % graalvmVersion,

    libraryDependencies += "org.graalvm.compiler" % "compiler" % graalvmVersion

  )
