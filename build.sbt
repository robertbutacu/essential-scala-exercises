lazy val hello = project
  .in(file("0-hello"))
  .dependsOn(utilities)
  .settings(commonSettings("hello"))

lazy val expressions = project
  .in(file("1-expressions"))
  .dependsOn(utilities)
  .settings(commonSettings("expressions"))

lazy val listExpressions = project
  .in(file("2-listExpressions"))
  .dependsOn(utilities)
  .settings(commonSettings("listExpressions"))

lazy val functionExpressions = project
  .in(file("3-functionExpressions"))
  .dependsOn(utilities)
  .settings(commonSettings("functionExpressions"))

lazy val filmographyPart1 = project
  .in(file("4-filmographyPart1"))
  .dependsOn(utilities)
  .settings(commonSettings("filmographyPart1"))

lazy val vector = project
  .in(file("5-vector"))
  .dependsOn(utilities)
  .settings(commonSettings("vector"))

lazy val counter = project
  .in(file("6-counter"))
  .dependsOn(utilities)
  .settings(commonSettings("counter"))

lazy val intList = project
  .in(file("7-intList"))
  .dependsOn(utilities)
  .settings(commonSettings("intList"))

lazy val intTree = project
  .in(file("x1-intTree"))
  .dependsOn(utilities)
  .settings(commonSettings("intTree"))

lazy val genericList = project
  .in(file("8-genericList"))
  .dependsOn(utilities)
  .settings(commonSettings("genericList"))

lazy val genericTree = project
  .in(file("x2-genericTree"))
  .dependsOn(utilities)
  .settings(commonSettings("genericTree"))

lazy val utilities = project
  .in(file("utilities"))
  .settings(commonSettings("utilities"))

lazy val root = project
  .in(file("."))
  .aggregate(
    hello,
    expressions,
    listExpressions,
    functionExpressions,
    filmographyPart1,
    vector,
    counter,
    intList,
    intTree,
    genericList,
    genericTree,
  )

def commonSettings(projectName: String) =
  Seq(
    name := projectName,
    scalaVersion := "2.12.7",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.1" % Test,
      "org.scalatest" %% "scalatest" % "3.0.1" % Test,
    ),
    scalacOptions ++= Seq(
      "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
      "-encoding", "utf-8",                // Specify character encoding used by source files.
      "-explaintypes",                     // Explain type errors in more detail.
      "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
      "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
      "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
      "-language:higherKinds",             // Allow higher-kinded types
      "-language:implicitConversions",     // Allow definition of implicit functions called views
      "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
      "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
      "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
      "-Xfuture",                          // Turn on future language features.
      "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
      "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
      "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
      "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
      "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
      "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
      "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
      "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
      "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
      "-Xlint:option-implicit",            // Option.apply used implicit view.
      "-Xlint:package-object-classes",     // Class or object defined in package object.
      "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
      "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
      "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
      "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
      "-Xlint:unsound-match",              // Pattern match may not be typesafe.
      "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
      "-Ypartial-unification",             // Enable partial unification in type constructor inference
      // "-Ywarn-dead-code",                  // Warn when dead code is identified.
      "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
      "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
      "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
      "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
      // "-Ywarn-numeric-widen",              // Warn when numerics are widened.
      "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
      // "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
      "-Ywarn-unused:locals",              // Warn if a local definition is unused.
      "-Ywarn-unused:params",              // Warn if a value parameter is unused.
      "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
      "-Ywarn-unused:privates",            // Warn if a private member is unused.
      "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
    )
  )
