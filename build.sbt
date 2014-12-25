name := "scala-differential-plots"

version := "1.0"

libraryDependencies += "com.github.wookietreiber" %% "scala-chart" % "latest.integration"

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq("-feature")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)
