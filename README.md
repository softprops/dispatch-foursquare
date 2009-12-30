# dispatch-foursquare

A scala [dispatch](http://databinder.net/dispatch/) interface for the [foursquare.com](http://foursquare.com/) api.

No fancy pants phone? No problem.

Check in from everywhere anywhere.

## usage

    import foursquare._
    import dispatch.liftjson.Js._
    import oauth._
    import Http._
    
    val fs = OAuthClient(
      Consumer("C0nSum3rK3y","C0nsuM3rS3cr3t"),
      Token("aCcessK3y","Acc3sSS3cr3t")
    )

    implicit val http = Http

    fs.call(
      Checkins checkIn at("Times Square") shouting("hello!") withTweet
    )

    val checkinHistory = fs.call(Checkins history) 

    val venues = fs.call(Venues near(40.759011,-73.984472))
  
    val friendsByName fs.call(Friends named("ryu"))
    
    val friendsOf = fs.call(Friends of(12345))
    
    val user = fs.call(Users get(12345) withBadges withoutMayorships)
  
2009 doug tangren [softprops]