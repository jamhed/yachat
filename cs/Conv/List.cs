define ["pi/Pi", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class ConvList extends aPi

   attr: -> super.concat ["view"]

   init: ->
      @skip = 1
      @sub "#bullet@user/conv_list", (ev, args) =>
         storedConvId = Cmon.conv_id()
         @empty()
         [status, List] = args
         tmpl = @rt.source @a.view
         @seen = 0
         for convId in List
            @e.append tmpl {id: convId}
            if convId == storedConvId
               @seen = 1
         if @skip
            @skip = 0
            if @seen
               @rpc "#bullet@conv_status", ["join", storedConvId]
            else
               @rpc "#bullet@conv_status", ["part"]
         else
            @skip = 1
         @rt.pi @e

      @sub "#bullet@conv/status/join", (ev, args) =>
         @rpc "#bullet@query_convs"

      @sub "#bullet@conv/status/part", (ev, args) =>
         @rpc "#bullet@query_convs"

      @wait_ajax_done =>
         @rpc "#bullet@query_convs"
