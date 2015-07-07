define ["Nsend"], (aPi) -> class Login extends aPi

	attr: -> super.concat ["target", "dialog"]

	init: ->
		super
		@e.click (ev) =>
			@brpc "self", [], (bullet) =>
				h = if bullet.fb_status == "connected" then fb_connect: 1 else fb_connect: 0

				@append @a.dialog, h
				@process @e
