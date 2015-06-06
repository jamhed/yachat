define ["Nsend", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class LoginStatus extends aPi

   attr: -> super.concat ["login", "logout"]

   logged: (user) ->
      @clear()
      console.log "XXX", user
      tmpl = mSource.get(@a.logout)
      @e.html tmpl display: Cmon.displayNameA user
      @rt.pi @e

   not_logged: ->
      @e.html mSource.get(@a.login)
      @rt.pi @e
 
   init: ->
      @sub "#bullet@user/status/registered", (ev, user) => @logged user

      @sub "#bullet@user/status/not_logged", (ev) => @not_logged()
     
      @sub "#bullet@user/status/anonymous", (ev, user) => @logged user
