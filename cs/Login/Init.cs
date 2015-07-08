define ["Nsend"], (Pi) -> class LoginInit extends Pi

	conn: false
	query_request: false

	init: ->
		super
		@bsub "conn/open", =>
			@conn = true
			if @query_request
				@query_request = false
				@query()

		@bsub "conn/close", =>
			@query_request = true	# re-read status on disconnects
			@conn = false

		@wait_ajax_done => @query()

		@sub "#router@hash/change", =>
			@wait_ajax_done => @query()

	query: ->
		if @conn 
			@debug "login/init", "query"
			@brpc "check_user_id"
		else
			@query_request = true