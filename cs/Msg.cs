define ["pi/Pi"], (Pi) -> class Msg extends Pi

   attr: -> super.concat ["target"]

   init: ->
      @e.keyup (e) =>
         if e.keyCode == 13
            @rpc @a.target, [@e.val()], => @e.val ""
