define ["Nsend"], (Pi) -> class LoginInit extends Pi

	init: ->
		super

		@wait_ajax_done =>
			return if @e.attr("processed") == "2"
			@brpc "check_user_id"
