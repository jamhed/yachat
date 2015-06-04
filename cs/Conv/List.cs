define ["pi/Pi", "pi/m/Source"], (aPi, mSource) -> class ConvList extends aPi

   attr: -> super.concat ["view"]

   init: ->
      @sub "#bullet@user/conv_list", (ev, args) =>
         [status, List] = args
         @empty()
         tmpl = @rt.source @a.view
         @e.append tmpl {id: conv} for conv in List
         @rt.pi @e

      @sub "#bullet@conv/status/part", (ev, args) =>
         @rpc "#bullet@query_convs"
