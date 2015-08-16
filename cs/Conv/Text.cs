define ["Nsend", "Cmon"], (Pi, Cmon) -> class ConvText extends Pi

	attr: -> super.concat ["row"]

	draw: (rows) ->
		@clear()
		for row in rows.reverse()
			[user, msg] = row
			@append user, msg

	query: -> @nsend ["conv/history", Cmon.sid(), Cmon.conv_id()], (status, rows) => @draw rows

	init: ->
		super
		@bsub "new_msg", (e, args) =>
			[convId, user, msg] = args
			if convId == Cmon.conv_id()
				@append user, msg

		@bsub "conv_msg", (e, args) =>
			[convId, [stamp, [msg, user]]] = args
			if convId == Cmon.conv_id()
				@append user, [stamp, msg]

		@bsub "conv/status/part", (e,args) => @clear()
		@bsub "conv/status/join", (e, args) => @query()

		@bsub "user/status", (ev, [status, args]) =>
			if status == "anonymous" or status == "registered"
				@query()
			else
				@clear()
				
	append: (user, msg) ->
		[stamp,text] = msg
		@e.prepend @tmpl @a.row, timestamp: stamp, username: Cmon.displayNameA(user), text: text