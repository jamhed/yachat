define ["pi/Pi"], (aPi) -> class Login extends aPi

   attr: -> super.concat ["target", "dialog"]

   init: ->
      @e.click (ev) =>
         @rt.append @a.dialog, {}
         @rt.pi @e
