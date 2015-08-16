define ["Nsend", "pi/m/Source"], (aPi, mSource) -> class ConvStatus extends aPi

	attr: -> super.concat ["join", "leave", "display"]

	init: ->
		super

		@bsub "conv/status/join", (ev, convId) =>
			$(@a.display).val convId
			@e.html @tmpl @a.leave, {}
			@process @e

		@bsub "conv/status/part", (ev, args) =>
			$(@a.display).val ""
			@e.html @tmpl @a.join, {}
			@process @e

		@bsub "user/status/not_logged", (ev, args) =>
			@e.html @tmpl @a.join, {}
			@process @e