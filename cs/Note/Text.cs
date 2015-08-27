define ["Nsend", "Cmon"], (Pi, Cmon) -> class NoteText extends Pi

	init: ->
		super
		@e.keyup (e) =>
			if e.keyCode == 13
				@nsend ["note/text", Cmon.sid(), @e.val()], (Status, Tid) => @handle_status(Status)

	handle_status: (Status) ->
		@debug Status