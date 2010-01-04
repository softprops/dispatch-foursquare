package dispatch

import org.specs._
  
object FoursquareSpec extends Specification {
  import foursquare._
  import dispatch.liftjson.Js._
  import oauth._
  import Http._
  
  val conf = new java.io.File("src/test/resources/foursquare.test.cfg")
  if(!conf.exists) println (
    """ |This test expects a file containing
        | oauth_consumer_key:"C0nSum3rK3y"
        | oauth_consumer_secret:"C0nsuM3rS3cr3t}"
        | oauth_access_key: "aCcessK3y"
        | oauth_access_secret: "Acc3sSS3cr3t"
        | @ src/test/resources/foursquare.test.cfg
        |
        | to create one
        | 1. grab a consumer key/sec @ http://foursquare.com/oauth/
        | 2. generate a request token with http(Auth.requestToken)
        | 3. use that token to generate a url to authorize
        |    http(Auth.authorizeUrl(reqToken))
        | 4. after authorizing the request token, get an access token with
        |    http(Auth.accessToken(consumer, reqToken))
        | 5. store the consumer key+secret and the accessKey+secret 
        |    in the file mentioned above
        |""" stripMargin)
  else {
    import _root_.net.lag.configgy.{ Configgy => C }
    
    C.configure(conf.getPath)
    val consumer = Consumer(
      C.config.getString("oauth_consumer_key").get, 
      C.config.getString("oauth_consumer_secret").get
    )
    val accessToken = Token(
      C.config.getString("oauth_access_key").get, 
      C.config.getString("oauth_access_secret").get
    )
    val client = OAuthClient(consumer, accessToken)
    
   detailedDiffs()
    
    implicit val http = new Http
      
    "Cities" should {
      "should find recently active cities" in {
         val res = client.call(Cities recentlyActive)
         val ids = for (r <- res; id <- City.id(r)) yield id
         ids.size must be > (0)
       }
      "find the foursquare city nearest 40.759011,-73.984472 (nyc)" in {
        val res = client.call(Cities near(40.759011,-73.984472)) 
       
        val List(name) = res.flatMap(City.name)
        val List(id) = res.flatMap(City.id)
        val List(shortName) = res.flatMap(City.shortName)
        val List(timezone) = res.flatMap(City.timezone)
        //val List(geolat) = res.flatMap(City.geolat)
        //val List(geolong) = res.flatMap(City.geolong)
        
        name must_== "New York"
        id must_== 22
        shortName must_== "NYC"
        timezone must_== "America/New_York"
        //geolat must be closeTo(40.759011, 0.003)
        //geolong must be closeTo(-73.984472, 0.003)
      }
    }
    
    "Checkins" should {
      "perform a checkin" in {
        // note you prob what to delete these from your history after running 
        // this test :)
        val res = client.call(Checkins.checkIn at("Times Square") shouting("testing dispatch-foursquare") privately)
        val List(msg) = res.flatMap(Checkin.message)
        msg must_=="OK! We've got you @ Times Square."
      }
      "find checkins near 40.759011,-73.984472 (nyc)" in {
          val res = client.call(Checkins near(40.759011,-73.984472))
          val ids = for (r <- res; id <- Checkin.id(r)) yield id
          ids.size must be > 0
      }
      "list a checkin history" in {
        val res = client.call(Checkins history)
        val ids = for (r <- res; id <- Checkin.id(r)) yield id
        ids.size must be > 0
      }
    }
    
    "Users" should {
       "get the user info for 15026" in {
        val res = client.call(Users get(15026))
        val List(id) = res.flatMap(User.id)
        id must_== 15026
      }
      "get the user info about the current user" in {
        val res = client.call(Users current)
        val List(id) = res.flatMap(User.id)
        id must_== 140048
      }
    }
    
    "Friends" should {
      "find user 140048's friends" in {
        val res = client.call(Friends of(140048))
        val ids = for (r <- res; id <- Checkin.id(r)) yield id
        ids.size must be > 0
      }
      "find friends by name" in {
        val res = client.call(Friends named("Ryan"))
        val ids = for (r <- res; id <- Checkin.id(r)) yield id
        ids must contain(15026) 
      }
    }
    
    "Venues" should {
      "find the venues near 40.759011,-73.984472 (nyc)" in {
        val res = client.call(Venues near(40.759011,-73.984472))
        val ids = for (r <- res; id <- Venue.id(r)) yield id
        ids.size must_== 0
      }
      "get venue details for nyc times square" in {
        val res = client.call(Venues get(41422))
        val List(id) = res.flatMap(Venue.id)
        id must_== 41422
      }
    }
  }
}