define ["pi/Pi", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class ConvList extends aPi

   attr: -> super.concat ["view"]

   draw: ->
      tmpl = @rt.source @a.view

      @empty()
      for convId in List
         @e.append tmpl {id: convId}
      
      @rt.pi @e

   init: ->
      @sub "#bullet@conv/status/join", (ev, args) =>
         @rpc "#bullet@query_convs"

      @sub "#bullet@conv/status/part", (ev, args) =>
         @rpc "#bullet@query_convs"

      @wait_ajax_done =>
         @rpc "#bullet@query_convs"
