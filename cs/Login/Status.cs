define ["pi/Pi", "pi/m/Source"], (aPi, mSource) -> class LoginStatus extends aPi

   attr: -> super.concat ["login", "logout"]

   init: ->
      @sub "#bullet@registered", (ev, args) =>
         [status, [userId, name, email]] = args
         displayName = if name then name else email
         tmpl = mSource.get(@a.logout)
         @e.html tmpl(display: displayName)
         @rt.pi @e
      @sub "#bullet@user/logout", (ev) =>
         @e.html mSource.get(@a.login)
         @rt.pi @e
