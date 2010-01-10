package dispatch.foursquare

import dispatch.oauth._
import dispatch.oauth.OAuth._

import dispatch.liftjson.Js._
import net.liftweb.json._
import net.liftweb.json.JsonAST._

import org.apache.commons.codec.binary.Base64

/** Client is a function to wrap API operations */
abstract class Client extends ((Request => Request) => Request) {
  import Http.builder2product
  val host = :/("api.foursquare.com") / "v1"
  def call[T](method: Method[T])(implicit http: Http) = {
    http(method.defaultHandler(apply(method)))
  }
}

case class OAuthClient(consumer: Consumer, accessToken: Token) extends Client {
  def apply(block: Request => Request): Request =
    block(host) <@ (consumer, accessToken)
}

case class BasicAuthClient(usernameOrEmail: String, password: String) extends Client {
  val authorization = "Basic " + new String(
    Base64.encodeBase64("%s:%s".format(usernameOrEmail, password).getBytes
  ))
  /** as(usernameOrEmail, password) will result in
   * WARN - Authentication error: Unable to respond to any of these challenges: {}
   */
  def apply(block: Request => Request): Request = 
    block(host) <:< Map("Authorization" -> authorization)
    
}

object Auth {
  val host = :/("www.foursquare.com")
  val svc = host / "oauth"
  def requestToken(consumer: Consumer) =
    svc.POST / "request_token" <@ consumer as_token      
  def authorizeUrl(reqToken: Token) = svc / "authorize" <<? reqToken
  def accessToken(consumer: Consumer, token: Token) = 
    svc.POST / "access_token" <@ (consumer, token) as_token
}

trait Method[T] extends Builder[Request => Request] {
  /** default handler used by Client#call. You can also apply the client 
      to a Method and define your own request handler. */
  def defaultHandler: Request => Handler[T]
}

object Cities extends CityBuilder(Map()) {
  /** get a list of recently active cities */
  def recentlyActive = new CitiesBuilder
  /** switch the auth user's default city */
  def switch(cityid: Long) = new CitySwitchBuilder(Map(
    "cityid" -> cityid
  ))
}

private [foursquare] class CitiesBuilder extends Method[List[JValue]] {
  def product = (_: Request) / "cities.json"
  def defaultHandler = _ ># ('cities ? ary)
}

private [foursquare] class CityBuilder(val params: Map[String, Any]) extends Method[List[JField]] {
  private def param(k: String)(v: Any) = new CityBuilder(params + (k -> v))
  
  /** get a City at or nearest geolat and geolong */
  def at(geolat: Double, geolong: Double) = 
     param("geolat")(geolat).param("geolong")(geolong)
  def at(geoLatLong: (Double,Double)) =
    param("geolat")(geoLatLong._1).param("geolong")(geoLatLong._2)
  
  def product = (_: Request) / "checkcity.json" <<? params
  def defaultHandler = _ ># ('city ? obj)
}

private [foursquare] class CitySwitchBuilder(val params: Map[String, Any]) extends Method[List[JField]] {
  def product = (_: Request) / "switchcity.json" << params
  def defaultHandler = _ ># 'data ? obj
}

object City {
  val id = 'id ? int
  val timezone = 'timezone ? str
  val name = 'name ? str
  val shortName = 'shortname ? str
  val geolat = 'geolat ? obj // TODO impl double in dispatch.liftjson.Js
  val geolong = 'geolong ? obj // ^ ^
}

object CitySwitch {
  // BUG?: parsed field comes back as JString rather than JInt List(JField(status,JString(1))
  val status = 'status ? str
  val message = 'message ? str
}

object Checkins extends CheckinsBuilder(Map()) {
  /** provides a means to checkin, shout, and post details */
  def checkIn = new CheckinBuilder(Map())
  /** get a list of checkins for the authed user */
  def history = new HistoryBuilder(Map())
}

private [foursquare] class CheckinsBuilder(val params: Map[String, Any]) extends Method[List[JValue]] {
  private def param(k: String)(v: Any) = new CheckinsBuilder(params + (k -> v))
  
  /** get a list friends of checkin's @ near a geolat + geolong */
  def at(geolat: Double, geolong: Double) = 
    param("geolat")(geolat).param("geolong")(geolong)
  def at(geoLatLong: (Double,Double)) =
    param("geolat")(geoLatLong._1).param("geolong")(geoLatLong._2)
  
  def product = (_: Request) / "checkins.json" <<? params 
  def defaultHandler = _ ># ('checkins ? ary)
}

private [foursquare] class CheckinBuilder(val params: Map[String, Any]) extends Method[List[JField]] {
  private def param(k: String)(v: Any) = new CheckinBuilder(params + (k -> v))
  
  def at(vid: Long) = param("vid")(vid)
  def at(venue: String) = param("venue")(venue)
  def at(geolat: Double, geolong: Double) = 
    param("geolat")(geolat).param("geolong")(geolong)
  def at(geoLatLong: (Double,Double)) =
    param("geolat")(geoLatLong._1).param("geolong")(geoLatLong._2)
  val shouting = param("shout")_
  def privately = param("private")(1)
  def publicly = param("private")(0)
  def withoutTweet = param("twitter")(0)
  def withTweet = param("twitter")(1)
  def notPostingToFacebook = param("facebook")(0)
  def postingToFacebook = param("facebook")(1)
  
  def product = (_: Request) / "checkin.json" << params 
  def defaultHandler = _ ># ('checkin ? obj)
}

private [foursquare] class HistoryBuilder(val params: Map[String, Any]) extends Method[List[JValue]] {
  private def param(k: String)(v: Any) = new HistoryBuilder(params + (k -> v))
  
  val limit = param("l")_
  val since = param("sinceid")_
  
  def product = (_: Request) / "history.json" <<? params 
  def defaultHandler = _ ># ('checkins ? ary)
}

object Checkin {
  val id = 'id ? int
  val user = 'user ? obj
  val venue = 'venue ? obj
  val display = 'display ? str
  val message = 'message ? str
  val shout = 'shout ? str
  val created = 'created ? date
  
  // extended
  val mayor = 'mayor ? obj
  
  object Mayor {
    val mtype = 'type ? str
    val checkins = 'checkins ? int
    val user = 'user ? obj
    val message = 'message ? str
  }
  
  val badges = 'badges ? ary
  val scoring = 'scoring ? ary
  
  object Score {
    val points = 'points ? int
    val icon = 'icon ? str
    val message = 'message ? str
  }
  
  val specials = 'specials ? ary 
  
  object Special {
    val id = 'id ? int
    val stype = 'type ? str
    val message = 'message ? str
  }
}

object Users extends UserBuilder(Map())
private [foursquare] class UserBuilder(params: Map[String, Any]) extends  Method[List[JField]] {
    private def param(k: String)(v: Any) = new UserBuilder(params + (k -> v))
    /** gets the authed user's info */
    def current = new UserBuilder(Map())
    
    /** gets a user's info by user id */
    val get = param("uid")_
    def withoutBadges = param("badges")(0)
    def withBadges = param("badges")(1)
    def withoutMayorships = param("mayor")(0)
    def withMayorships = param("mayor")(1)
    
    def product = (_: Request) / "user.json" <<? params
    def defaultHandler = _ ># ('user ? obj)
}

object Friends extends FriendsBuilder(Map()) {
  
  def ofMe = this
  
  def named(name: String) = new FriendsByNameBuilder(Map(
    "q" -> name
  ))
  def withPhone(number: String) = new FriendsByPhoneBuilder(Map(
    "q" -> number
  ))
  def withTwitterName(screenName: String) = new FriendsByTwitterBuilder(Map(
    "q" -> screenName
  ))
}
private [foursquare] class FriendsBuilder(params: Map[String, Any]) extends Method[List[JValue]] {
  private def param(k: String)(v: Any) = new FriendsBuilder(params + (k -> v))
  
  val of = param("uid")_
  
  def product = (_: Request) / "friends.json" <<? params
  def defaultHandler = _ ># ('friends ? ary)
}

private [foursquare] class FriendsByNameBuilder(params: Map[String, Any]) extends  Method[List[JValue]] {
  def product = (_: Request) / "findfriends" / "byname.json" <<? params
  def defaultHandler = _ ># ('friends ? ary)
}

private [foursquare] class FriendsByPhoneBuilder(params: Map[String, Any]) extends  Method[List[JValue]] {
  def product = (_: Request) / "findfriends" / "byphone.json" <<? params
  def defaultHandler = _ ># ('friends ? ary)
}

private [foursquare] class FriendsByTwitterBuilder(params: Map[String, Any]) extends  Method[List[JValue]] {
  def product = (_: Request)  / "findfriends" / "bytwitter.json" <<? params
  def defaultHandler = _ ># ('friends ? ary)
}

object Venues extends VenuesBuilder(Map()) {
  def get(vid: Long) = new VenueBuilder(Map(
    "vid" -> vid
  ))
  def add = new VenueMakerBuilder(Map())
  def proposeEditTo(vid: Long) = new EditVenueBuilder(Map(
    "vid" -> vid
  ))
  def close(vid: Long) = new CloseVenueBuilder(Map(
    "vid" -> vid
  ))
}

private [foursquare] class VenuesBuilder(params: Map[String, Any]) extends Method[List[JValue]] {
  private def param(k: String)(v: Any) = new VenuesBuilder(params + (k -> v))
  
  def at(geolat: Double, geolong: Double) = 
    param("geolat")(geolat).param("geolong")(geolong)
  def at(geoLatLong: (Double,Double)) =
    param("geolat")(geoLatLong._1).param("geolong")(geoLatLong._2)
    
  val limit = param("l")_
  val named = param("q")_
  
  def product = (_: Request) / "venues.json" <<? params
  def defaultHandler = _ ># ('venues ? ary)
}

private [foursquare] class VenueBuilder(params: Map[String, Any]) extends Method[List[JValue]] {  
  def product = (_: Request) / "venue.json" <<? params
  def defaultHandler = _ ># ('venue ? obj)
}

private [foursquare] class VenueMakerBuilder(params: Map[String, Any]) extends Method[List[JField]] {
  private def param(k: String)(v: Any) = new VenueMakerBuilder(params + (k -> v))
  
  val name = param("name")_
  val address = param("address")_
  val crossStreet = param("crossstreet")_
  val city = param("city")_
  val zip = param("zip")_
  val cityId = param("cityid")_
  val phone = param("phone")_
  def at(geolat: Double, geolong: Double) = 
    param("geolat")(geolat).param("geolong")(geolong)
  def at(geoLatLong: (Double,Double)) =
    param("geolat")(geoLatLong._1).param("geolong")(geoLatLong._2) 
  def product = (_: Request) / "addvenue.json" << params
  def defaultHandler = _ ># ('venue ? obj)
}

private [foursquare] class EditVenueBuilder(params: Map[String, Any]) extends VenueMakerBuilder(params) {
  override def product = (_: Request) / "venue" / "proposeedit.json" << params
}

private [foursquare] class CloseVenueBuilder(params: Map[String, Any]) extends Method[List[String]] {
  def product = (_: Request) / "venues" / "flagclosed.json" << params
  def defaultHandler = _ ># ('response ? str)
}

object Tips extends TipsBuilder(Map()) {
  /** adds a new Tip */
  def add = new TipBuilder(Map())
  /** changes a Tip to a Todo */
  def toTodo(tid: Long) = new TipChangeBuilder(Map(
    "tid" -> tid
  ))
  /** flags a Todo as done */
  def markDone(tid: Long) = new TipDoneBuilder(Map(
    "tid" -> tid
  ))
}

private [foursquare] class TipsBuilder(params: Map[String, Any]) extends Method[List[JValue]] {
  private def param(k: String)(v: Any) = new TipsBuilder(params + (k -> v))
  
  def at(geolat: Double, geolong: Double) = 
    param("geolat")(geolat).param("geolong")(geolong)
  def at(geoLatLong: (Double,Double)) =
    param("geolat")(geoLatLong._1).param("geolong")(geoLatLong._2)
  val limit = param("l")_
  
  def product = (_: Request) / "tips.json" <<? params
  def defaultHandler = _ ># ('group ? ary)
}

private [foursquare] class TipBuilder(params: Map[String, Any]) extends Method[List[JField]] {
  private def param(k: String)(v: Any) = new TipBuilder(params + (k -> v))
  
  def at(vid: Long) = param("vid")(vid)
  def at(geolat: Double, geolong: Double) = 
    param("geolat")(geolat).param("geolong")(geolong)
  def at(geoLatLong: (Double,Double)) =
    param("geolat")(geoLatLong._1).param("geolong")(geoLatLong._2)
  val text = param("text")_
  val asTip = param("type")("tip")
  val asTodo = param("type")("todo")
  
  def product = (_: Request) / "addtip.json" << params
  def defaultHandler = _ ># ('tip ? obj)
}

private [foursquare] class TipChangeBuilder(params: Map[String, Any]) extends Method[List[JField]] {
  def product = (_: Request) / "tip" / "marktodo.json" << params
  def defaultHandler = _ ># ('tip ? obj)
}

private [foursquare] class TipDoneBuilder(params: Map[String, Any]) extends Method[List[JField]] {
  override def product = (_: Request) / "tip" / "markdone.json" << params
  def defaultHandler = _ ># ('tip ? obj)
}

object Badge {
  val id = 'id ? int
  val name = 'name ? str
  val icon = 'icon ? str
  val description = 'description ? str
}

object User {
  val id = 'id ? int
  val firstname = 'firstname ? str
  val lastname = 'lastname ? str
  val gender = 'gender ? str
  val photo = 'photo ? str

  // extended
  val city = 'city ? obj
  val phone = 'phone ? str
  val email = 'email ? str
  val twitter = 'twitter ? str
  val facebook = 'facebook ? str
  val friendstatus = 'friendstatus ? str
  val checkin = 'checking ? obj
  val badges = 'badges ? ary
  
  // if self
  val settings = 'settings ? ary
}

object Venue {
  val id = 'id ? int
  val name = 'name ? str
  val address = 'address ? str
  val geolat = 'geolat ? int
  val geolong = 'geolong ? int
  
  // extended
  val crossStreet = 'crossstreet ? str
  val city = 'city ? str
  val state = 'state ? str
  val zip = 'zip ? str
  val phone = 'phone ? str
  val twitter = 'twitter ? str
  val stats = 'stats ? obj 

  object Stats {
    val checkins = 'checkins ? int
    val beenHere = 'beenhere ? obj
  
    object Been {
      def me = 'me ? str // bool
      def myFriends = 'friends ? str // bool
    }
  }
  
  val checkins = 'checkins ? ary
  val tips = 'tips ? ary
  val tags = 'tags ? ary
  
  object Tag { def txt = 'tag ? str }
  
  // on edit
  val response = 'response ? str
}

object Tip {
  val id = 'id ? int
  val txt = 'text ? str
  val url = 'url ? str
  val created = 'created ? date
  val user = 'user ? obj
}