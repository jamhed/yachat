define ["pi/Pi", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class ConvList extends aPi

   attr: -> super.concat ["view"]

   init: ->
      @sub "#bullet@conv/users", (ev, args) =>
         @empty()
         [status, List] = args
         tmpl = @rt.source @a.view
         @e.append tmpl {id: id, display: Cmon.displayName(id,name,email) } for [id,name,email] in List
         @rt.pi @e

      @sub "#bullet@conv/status/join", (ev, args) =>
         @rpc "#bullet@query_conv_users"

      @sub "#bullet@conv/status/part", (ev, args) =>
         @empty()

