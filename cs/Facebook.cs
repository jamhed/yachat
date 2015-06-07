define ["Nsend", "//connect.facebook.net/en_US/sdk.js"], (Pi, Facebook) -> class Facebook extends Pi

   attr: -> super.concat ["fbid"]

   init: ->
      FB.init
         appId: @a.fbid
         xfbml: true,
         version: "v2.3"

      FB.getLoginStatus => (response) =>
         console.log "FACEBOOK", response
