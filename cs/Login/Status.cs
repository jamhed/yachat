define ["pi/Pi", "pi/m/Source"], (aPi, mSource) -> class LoginStatus extends aPi

   attr: -> super.concat ["login", "logout"]

   init: ->
      @sub "#bullet@user/status/registered", (ev, args) =>
         [userId, name, email] = args
         displayName = if name then name else email
         tmpl = mSource.get(@a.logout)
         @e.html tmpl(display: displayName)
         @rt.pi @e

      @sub "#bullet@user/status/not_logged", (ev) =>
         @e.html mSource.get(@a.login)
         @rt.pi @e
      
      @sub "#bullet@user/status/anonymous", (ev) =>
         @e.html mSource.get(@a.login)
         @rt.pi @e
