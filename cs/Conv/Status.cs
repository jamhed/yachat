define ["pi/Pi", "pi/m/Source"], (aPi, mSource) -> class ConvStatus extends aPi

   attr: -> super.concat ["join", "leave"]

   init: ->
      @sub "#bullet@conv/join", (ev, args) =>
         @e.html mSource.get(@a.leave)
         @rt.pi @e
      @sub "#bullet@conv/new", (ev, args) =>
         @e.html mSource.get(@a.leave)
         @rt.pi @e
      @sub "#bullet@conv/leave", (ev) =>
         @e.html mSource.get(@a.join)
         @rt.pi @e
