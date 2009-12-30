package dispatch

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
  
class FoursquareSpec extends Spec with ShouldMatchers {
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
    
    implicit val http = new Http
      
    describe("Cities") {
      it("should find recently active cities") {
        val cities = client.call(Cities recentlyActive)
        cities.size should be > (0)
        val names = cities map City.name
        println("name %s " format names.size)
      }
      it("should find the foursquare city nearest 40.759011,-73.984472 (nyc)") {
        val city = client.call(Cities near(40.759011,-73.984472)) 
        println("city near nyc -> %s" format(city))
      }
    }
    
    describe("Checkins") {
      it("should find checkins near ?") {
        val checkins = client.call(Checkins near(40.759011,-73.984472))
        println("checkins near 40.759011,-73.984472 %s" format checkins)
      }
      it("should perform a checkin") {
        // note you prob what to delete these from your history after running 
        // this test :)
        val checkin = client.call(Checkins.checkIn at("Times Square") shouting("testing dispatch-foursquare") privately)
        println("checking in privately @ %s" format(checkin map Checkin.message))
      }
      it("should list a checkin history") {
        val checkins = client.call(Checkins history)
        println("checkins history %s" format(checkins map Checkin.shout))
      }
    }
    
    describe("users") {
      it("should get the user info for 15026") {
        val user = client.call(Users get(15026))
        println("user 15026 is %s" format user)
      }
      it("should get the user info about the current user") {
        val user = client.call(Users current)
        println("you are %s" format user)
      }
    }
    
    describe("friends") {
      it("should find user 15026's friends") {
        val friends = client.call(Friends of(15026))
        println("friends of 15026 are %s" format friends)
      }
      it("should find friends by twitter name") {
        val friends = client.call(Friends withTwitterName("bjburton"))
        println("friends by twitter are %s" format friends)
      }
    }
    
    describe("venues") {
      it("should find the venues near 40.759011,-73.984472 (nyc)") {
        val venues = client.call(Venues near(40.759011,-73.984472))
        println("venues near nyc -> %s" format venues)
      }
      it("should get venue details for nyc times square") {
        val venue = client.call(Venues get(41422))
        println("venue details -> %s" format venue)
      }
    }
  }
}