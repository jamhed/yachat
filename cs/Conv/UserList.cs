define ["Nsend", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class ConvList extends aPi

   attr: -> super.concat ["view"]

   draw: (List) ->
      @empty()
      tmpl = @rt.source @a.view
      @e.append tmpl {id: id, display: Cmon.displayName(id,name,email) } for [id,name,email] in List
      @rt.pi @e

   query: ->
      @nsend ["conv/users", Cmon.user_id(), Cmon.conv_id()], (status, List) => @draw List

   init: ->
      @sub "#bullet@conv/status/join", (ev, args) => @query()

      @sub "#bullet@conv/status/part", (ev, args) => @empty()

      @sub "#bullet@user/status/registered", (ev, args) => @query()

      @sub "#bullet@user/status/anonymous", (ev, args) => @query()

      @sub "#bullet@user/status/not_logged", (ev, args) => @empty()


