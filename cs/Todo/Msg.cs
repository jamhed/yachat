define ["Nsend", "Cmon"], (Pi, Cmon) -> class TodoMsg extends Pi

	init: ->
		super
		@e.keyup (e) =>
			if e.keyCode == 13
				@nsend ["todo/add", [Cmon.sid()], @e.val()], (Status, Tid) => @handle_status(Status, Tid)

	handle_status: (Status, Tid) ->
		if Status == "fail"
			@info "Unable to add entry. Please add or select default list."
		else
			@rpc "#lists@query"
			@e.val ""