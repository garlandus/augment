val scala3Version = "3.3.1"

lazy val FunTest = config("fun") extend(Test)
def itFilter(name: String): Boolean = name endsWith "ITest"
def unitFilter(name: String): Boolean = (name endsWith "Test") && !itFilter(name)

lazy val root = project
  .in(file("."))
  .configs(FunTest)
  .configs(IntegrationTest)
  .settings(
    name := "augment",
    version := "0.0.2",
    scalaVersion := scala3Version,

    // zero-dependency library: these are loaded only for testing
    libraryDependencies ++= Seq (
      "org.scalameta" %% "munit" % "1.0.0-M10" % "it,test",
      "org.junit.jupiter" % "junit-jupiter-api" % "5.10.1" % "it,test",
      "com.github.sbt" % "junit-interface" % "0.13.3" % "it,test",
      "org.apache.commons" % "commons-math3" % "3.6.1" % "it,test",
      "com.google.guava" % "guava" % "32.1.3-jre" % "it,test",
      "info.debatty" % "java-string-similarity" % "2.0.0" % "it,test",
      "org.typelevel" %% "cats-effect" % "3.5.2" % "it,test",
      "dev.zio" %% "zio" % "2.0.19" % "it,test",
      "dev.zio" %% "zio-prelude" % "1.0.0-RC21" % "it,test",
      "dev.zio" %% "zio-direct" % "1.0.0-RC7" % "it,test",
    ),

    Global / onChangedBuildSource := ReloadOnSourceChanges,
    inConfig(FunTest)(Defaults.testTasks),
    inConfig(IntegrationTest)(Defaults.testSettings),
    FunTest / testOptions := Seq(Tests.Filter(itFilter)),
)
 
