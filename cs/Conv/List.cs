define ["pi/Pi", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class ConvList extends aPi

   attr: -> super.concat ["view"]

   init: ->
      @skip = 1
      @sub "#bullet@user/conv_list", (ev, args) =>
         storedConvId = Cmon.conv_id()
         @empty()
         [status, List] = args
         tmpl = @rt.source @a.view
         for convId in List
            @e.append tmpl {id: convId}
            if @skip
               @skip = 0
               @rpc "#bullet@conv_status", ["join", convId] if convId == storedConvId
         @rt.pi @e

      @sub "#bullet@conv/status/join", (ev, args) =>
         @rpc "#bullet@query_convs"

      @sub "#bullet@conv/status/part", (ev, args) =>
         @rpc "#bullet@query_convs"

