define ["Nsend", "Cmon"], (Pi, Cmon) -> class Msg extends Pi

	init: ->
		super
		@e.keyup (e) =>
			if e.keyCode == 13
				@send "msg/conv", Cmon.sid(), Cmon.conv_id(), @e.val()
				@e.val ""
