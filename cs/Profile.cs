define ["Nsend", "Cmon"], (Pi, Cmon) -> class Profile extends Pi

	attr: -> super.concat ["template"]

	init: ->
		super
		@bsub "user/profile", (ev, [status, userInfo]) => @draw userInfo
		@wait_ajax_done () => @query()
	
	draw: (u) ->
		@debug "draw"
		@e.html @tmpl @a.template, u
		@process()

	query: ->
		@nsend ["user/get", Cmon.sid()], (status, sessionId, userInfo) => @draw userInfo

	delete: ->
    	@nsend ["user/delete", Cmon.sid()], =>
        	Cmon.set_conv_id null
        	window.location = "#"

	update: (List)  ->
		h = Cmon.list2hash List
		@nsend ["user/update", Cmon.sid(), h], (status, a) =>
			if status == "ok"
				@info "Profile was updated."
				@rpc "#bullet@pub_event", ["user/status", ["registered", h]]
			else
				@error "Profile wasn't updated."