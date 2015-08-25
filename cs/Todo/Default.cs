define ["Nsend", "Cmon"], (Pi, Cmon) -> class TodoDefault extends Pi

	attr: ->
		super.concat ["id", "target"]

	init: ->
		super
		@e.change (ev) =>
			state = @e.is ":checked"
			@nsend ["todo/default", [Cmon.sid()], parseInt(@a.id), state], () =>
				@rpc @a.target if @a.target