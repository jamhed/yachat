define ["Nsend", "pi/m/Source"], (aPi, mSource) -> class ConvStatus extends aPi

   attr: -> super.concat ["join", "leave", "display"]

   init: ->
      super

      @bsub "conv/status/join", (ev, convId) =>
         $(@a.display).val convId
         @e.html mSource.get(@a.leave)
         @rt.pi @e

      @bsub "conv/status/part", (ev, args) =>
         $(@a.display).val ""
         @e.html mSource.get(@a.join)
         @rt.pi @e

      @bsub "user/status/not_logged", (ev, args) =>
         @e.html mSource.get(@a.join)
         @rt.pi @e