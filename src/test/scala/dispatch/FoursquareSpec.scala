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
    val fs = OAuthClient(consumer, accessToken)
    
   detailedDiffs()
    
    implicit val http = new Http
      
    val Tokyo = 120l
    val TimesSquareVenue = 41422
    case class FSCity(id: Long, name: String, shortName: String, timezone: String, geo:(Double,Double))
    val TestCity = FSCity(22, "New York City", "NYC", "America/New_York", (40.759011, -73.984472))  
    case class FSUser(id: Long, firstname: String, lastname: String)
    val AFriend = FSUser(15026, "Ryan", "Gravener")
    val Me = FSUser(140048, "doug", "tangren")
      
    "Cities" should {
      "should find recently active cities" in {
         val res = fs.call(Cities recentlyActive)
         val ids = for (r <- res; id <- City.id(r)) yield id
         //val names = for (r <- res; name <- City.name(r)) yield name
         //println("cities %s" format(ids zip names))
         ids.size must be > (0)
       }
      "find the foursquare city nearest 40.759011,-73.984472 (nyc)" in {
        val res = fs.call(Cities at(TestCity.geo))
        val List(name) = res.flatMap(City.name)
        val List(id) = res.flatMap(City.id)
        val List(shortName) = res.flatMap(City.shortName)
        val List(timezone) = res.flatMap(City.timezone)
        //val List(geolat) = res.flatMap(City.geolat)
        //val List(geolong) = res.flatMap(City.geolong)
        
        name must_== TestCity.name
        id must_== TestCity.id
        shortName must_== TestCity.shortName
        timezone must_== TestCity.timezone
        //geolat.toString.toDouble must be closeTo(TimesSquareGeo._1, 0.003)
        //geolong.toString.toDouble must be closeTo(TimesSquareGeo._2, 0.003)
      }
      "switch to other cities" in {
        List(Tokyo, TestCity.id).foreach((id: Long) => {
          val res = fs.call(Cities switch(id))
          val List(status) = res.flatMap(CitySwitch.status)
          status must_== "1"
        })
      }
    }
  
    "Checkins" should {
      "perform a checkin" in {
        val res = fs.call(Checkins
          .checkIn 
          .at("Times Square") 
          .shouting("testing dispatch-foursquare") 
          .privately
        )
        val List(msg) = res.flatMap(Checkin.message)
        msg must_=="OK! We've got you @ Times Square."
      }
      "find checkins near 40.759011,-73.984472 (nyc)" in {
          val res = fs.call(Checkins at(TestCity.geo))
          val ids = for (r <- res; id <- Checkin.id(r)) yield id
          //val fnames = for (r <- res; List(u) <- r; fname <- User.firstname(u.asInstanceOf[jValue])) yield fname
          ids.size must be > 0
      }
      "list a checkin history" in {
        val res = fs.call(Checkins history)
        val ids = for (r <- res; id <- Checkin.id(r)) yield id
        ids.size must be > 0
      }
    }
    
    "Users" should {
       "get the user info for 15026" in {
          val res = fs.call(Users get(15026))
          val List(id) = res.flatMap(User.id)
          val List(firstname) = res.flatMap(User.firstname)
          val List(lastname) = res.flatMap(User.lastname)
          firstname must_== AFriend.firstname
          lastname must_== AFriend.lastname
          id must_== AFriend.id
        }
        "get the user info about the current user" in {
          val res = fs.call(Users current)
          val List(id) = res.flatMap(User.id)
          val List(firstname) = res.flatMap(User.firstname)
          val List(lastname) = res.flatMap(User.lastname)
          firstname must_== Me.firstname
          lastname must_== Me.lastname
          id must_== Me.id
        }
      }
    // 
    // "Friends" should {
    //   "find the current user's friends" in {
    //     val res = fs.call(Friends ofMe)
    //     val ids = for (r <- res; id <- Checkin.id(r)) yield id
    //     ids.size must be > 0
    //     // todo check settings
    //   }
    //   "find user 140048's friends" in {
    //     val res = fs.call(Friends of(140048))
    //     val ids = for (r <- res; id <- Checkin.id(r)) yield id
    //     ids.size must be > 0
    //   }
    //   "find friends by name" in {
    //     val res = fs.call(Friends named("Ryan"))
    //     val ids = for (r <- res; id <- Checkin.id(r)) yield id
    //     ids must contain(15026) 
    //   }
    // }
    
    "Venues" should {
        "find the venues near 40.759011,-73.984472 (nyc)" in {
          val res = fs.call(Venues at(TestCity.geo))
          val ids = for (r <- res; id <- Venue.id(r)) yield id
          ids.size must_== 0
        }
      "get venue details for nyc times square" in {
        val res = fs.call(Venues get(TestCity.id))
        val List(id) = res.flatMap(Venue.id)
        id must_== TestCity.id
      }
    //   "add a venue" {
    //     val res = fs.call(Venues.add
    //       .name("Doug's apartment")
    //       .address("...")
    //       .crossstreet("1st & 88th")
    //       .city("New York")
    //       .state("NY")
    //     )
    //     val List(name) = res.flatMap(Venue.name)
    //     name must_== "Doug's apartment"
    //   }
    //   "close a venue" {
    //     val res = fs.call(Venues close())
    //     val List(msg) = res
    //     msg must_== "ok"
    //   }
    }
    
    // "Tips" should {
    //   "find tips near 40.759011,-73.984472 (nyc)" in {
    //     val res = fs.call(Tips at(TestCity.geo))
    //     println("tips -> %s" format res)
    //     val ids = for (r <- res; id <- Tip.id(r)) yield id
    //     ids.size must be > 0
    //   }
    //   "add a new tip at 40.759011,-73.984472" in {
    //     val res = fs.call(Tip.add
    //       .at(TimesSquareGeo) 
    //       .text("test tip")
    //       .asTip
    //     )
    //     val List(txt) = res.flatMap(Tip.text)
    //     txt must_=="test tip"
    //   }
    //}
  }
}