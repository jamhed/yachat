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

		@bsub "user/status", (ev, [status, args]) =>
			if status == "anonymous" or status == "registered"
				@query()
			else
				@clear()