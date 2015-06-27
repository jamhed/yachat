define ["Nsend", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class LoginStatus extends aPi

   attr: -> super.concat ["login", "logout"]

   logged: (user) ->
      @clear()
      tmpl = mSource.get(@a.logout)
      @e.html tmpl display: Cmon.displayNameA user
      @rt.pi @e

   not_logged: ->
      @clear()
      @e.html mSource.get(@a.login)
      @rt.pi @e
 
   init: ->
      super
      
      @bsub "user/status/registered", (ev, user) => @logged user
      @bsub "user/status/not_logged", (ev) => @not_logged()
      @bsub "user/status/anonymous", (ev, user) => @logged user
