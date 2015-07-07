define ["Nsend"], (Pi) -> class LoginInit extends Pi

	init: ->
		super
		@wait_ajax_done => @query()

		@sub "#router@hash/change", =>
			@wait_ajax_done => @query()

	query: -> @brpc "check_user_id"