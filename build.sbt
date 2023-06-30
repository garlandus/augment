val scala3Version = "3.3.0"

lazy val FunTest = config("fun") extend(Test)
def itFilter(name: String): Boolean = name endsWith "ITest"
def unitFilter(name: String): Boolean = (name endsWith "Test") && !itFilter(name)

lazy val root = project
  .in(file("."))
  .configs(FunTest)
  .configs(IntegrationTest)
  .settings(
    name := "augment",
    version := "0.0.1",
    scalaVersion := scala3Version,

    // zero-dependency library: these are loaded only for testing
    libraryDependencies ++= Seq (
      "org.scalameta" %% "munit" % "0.7.29" % "it,test",
      "junit" % "junit" % "4.13.2" % "it,test",
      "com.novocode" % "junit-interface" % "0.11" % "it,test",
      "org.apache.commons" % "commons-math3" % "3.6.1" % "it,test",
      "com.google.guava" % "guava" % "28.1-jre" % "it,test",
      "info.debatty" % "java-string-similarity" % "2.0.0" % "it,test",
      "org.typelevel" %% "cats-effect" % "3.5.0" % "it,test",
      "dev.zio" %% "zio" % "2.0.13" % "it,test",
      "dev.zio" %% "zio-prelude" % "1.0.0-RC19" % "it,test",
      "dev.zio" %% "zio-direct" % "1.0.0-RC7" % "it,test",
    ),

    Global / onChangedBuildSource := ReloadOnSourceChanges,
    inConfig(FunTest)(Defaults.testTasks),
    inConfig(IntegrationTest)(Defaults.testSettings),
    FunTest / testOptions := Seq(Tests.Filter(itFilter)),
)
 
