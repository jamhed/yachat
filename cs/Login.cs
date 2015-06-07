define ["pi/Pi"], (aPi) -> class Login extends aPi

   attr: -> super.concat ["target", "dialog"]

   init: ->
      @e.click (ev) =>
         @rpc "#bullet@self", [], (bullet) =>
            h = if bullet.fb_status == "connected" then fb_connect: 1 else fb_connect: 0

            @rt.append @a.dialog, h
            @rt.pi @e
