define ["Nsend", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class ConvList extends aPi

   attr: -> super.concat ["view"]

   draw: (List) ->
      @clear()
      tmpl = @rt.source @a.view
      @e.append tmpl {id: User.id, display: Cmon.displayNameA User } for User in List
      @rt.pi @e

   query: ->
      @nsend ["conv/users", Cmon.sid(), Cmon.conv_id()], (status, List) => @draw List

   init: ->
      @sub "#bullet@conv/status/invite", (ev, args) => @query()

      @sub "#bullet@conv/status/join", (ev, args) => @query()

      @sub "#bullet@conv/status/part", (ev, args) => @clear()

      @sub "#bullet@user/status/registered", (ev, args) => @query()

      @sub "#bullet@user/status/anonymous", (ev, args) => @query()

      @sub "#bullet@user/status/not_logged", (ev, args) => @clear()


