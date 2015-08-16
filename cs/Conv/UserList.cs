define ["Nsend", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class ConvList extends aPi

	attr: -> super.concat ["view"]

	draw: (List) ->
		@clear()
		tmpl = @tmpl @a.view
		@e.append tmpl {id: User.id, display: Cmon.displayNameA User } for User in List
		@process @e

	query: ->
		@nsend ["conv/users", Cmon.sid(), Cmon.conv_id()], (status, List) => @draw List

	init: ->
		super
		@bsub "conv/update", (ev, args) => @query()
		@bsub "conv/status/invite", (ev, args) => @query()
		@bsub "conv/status/join", (ev, args) => @query()
		@bsub "conv/status/part", (ev, args) => @clear()
		@bsub "user/status/registered", (ev, args) => @query()
		@bsub "user/status/anonymous", (ev, args) => @query()
		@bsub "user/status/not_logged", (ev, args) => @clear()
