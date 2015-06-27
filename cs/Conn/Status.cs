define ["Nsend"], (Pi) -> class ConnStatus extends Pi

	init: ->
		super
		@bsub "conn/open", => @e.css color: "#fff"
		@bsub "conn/close", => @e.css color: "#aaa"

