import sbt._

class FoursquareProject(info: ProjectInfo) extends DefaultProject(info) {
  val lagNet = "lag.net repository" at "http://www.lag.net/repo"
  val databinderNet = "databinder.net repository" at "http://databinder.net/repo"
 
  val dpVersion = "0.7.4"
  val dispatchOauth = "net.databinder" %% "dispatch-oauth" % dpVersion
 
  // json 
  val dispatchLiftJson = "net.databinder" %% "dispatch-lift-json" % dpVersion
  val dispatchJson = "net.databinder" %% "dispatch-json" % dpVersion
  
  val (lj_org, lj_name, lj_version) = ("net.liftweb", "lift-json", "2.0-M5")
  val lift_json =
    if (buildScalaVersion startsWith "2.7.") lj_org % lj_name % lj_version
    else lj_org %% lj_name % lj_version
  //val liftJson = "net.liftweb" %% "lift-json" % "2.0-M5"
  
  // http
  //val dispatchMime = "net.databinder" %% "dispatch-mime" % dbVersion

  // testing
  val configgy = "net.lag" % "configgy" % "1.4" intransitive()
  val snapshots = "Scala Tools Snapshots" at "http://www.scala-tools.org/repo-snapshots/"
  val specs = "org.scala-tools.testing" % "specs" % "1.6.2.1" % "test"
}