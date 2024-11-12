val scala3Version = "3.5.2"

lazy val FunTest = config("fun") extend(Test)
def itFilter(name: String): Boolean = name endsWith "ITest"
def unitFilter(name: String): Boolean = (name endsWith "Test") && !itFilter(name)

lazy val root = project
  .in(file("."))
  .configs(FunTest)
  .configs(IntegrationTest)
  .settings(
    name := "augment",
    version := "0.0.4",
    scalaVersion := scala3Version,

	  // this is largely a zero-dependency library, but for now both Cats Effect and ZIO are included to simplify usage
    libraryDependencies ++= Seq(
      "org.clojure" % "clojure" % "1.12.0",
      "org.scalameta" %% "munit" % "1.0.2" % "it,test",
      "org.junit.jupiter" % "junit-jupiter-api" % "5.11.3" % "it,test",
      "com.github.sbt" % "junit-interface" % "0.13.3" % "it,test",
      "org.apache.commons" % "commons-math3" % "3.6.1" % "it,test",
      "com.google.guava" % "guava" % "33.3.1-jre" % "it,test",
      "info.debatty" % "java-string-similarity" % "2.0.0" % "it,test",
      "org.typelevel" %% "cats-effect" % "3.5.5",
      "dev.zio" %% "zio" % "2.1.12",
      "dev.zio" %% "zio-direct" % "1.0.0-RC7" % "it,test",
    ),

    Global / onChangedBuildSource := ReloadOnSourceChanges,
    inConfig(FunTest)(Defaults.testTasks),
    inConfig(IntegrationTest)(Defaults.testSettings),
    FunTest / testOptions := Seq(Tests.Filter(itFilter)),
)

