define ["Nsend", "Cmon"], (Pi, Cmon) -> class TodoMsg extends Pi

	init: ->
		super
		@e.keyup (e) =>
			if e.keyCode == 13
				@nsend ["todo/add", Cmon.sid(), @e.val()], (Status) => @rpc "#lists@query"
				@e.val ""