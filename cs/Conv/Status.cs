define ["pi/Pi", "pi/m/Source"], (aPi, mSource) -> class ConvStatus extends aPi

   attr: -> super.concat ["join", "leave", "display"]

   init: ->
      @sub "#bullet@conv/status/join", (ev, convId) =>
         $(@a.display).val convId
         @e.html mSource.get(@a.leave)
         @rt.pi @e

      @sub "#bullet@conv/status/part", (ev, args) =>
         $(@a.display).val ""
         @e.html mSource.get(@a.join)
         @rt.pi @e

      @sub "#bullet@user/status/not_logged", (ev, args) =>
         @e.html mSource.get(@a.join)
         @rt.pi @e
