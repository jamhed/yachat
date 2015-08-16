define ["Nsend", "Cmon"], (Pi, Cmon) -> class ProfileAvatar extends Pi

	init: ->
		super
		
		@bsub "sys/avatar/change", (e, avatarId) => @query()

		@wait_ajax_done => @query()

	draw: (avatar) ->
		@debug avatar
		@clear()
		if avatar?
			stamp = Date.now()
			[id,type] = avatar
			im = $("<img>").attr("src", "/store/avatar/#{id}?#{stamp}").addClass "img-responsive" 
			@e.append im

	query: -> @nsend ["user/avatar", Cmon.sid()], (id, type, mime) => @draw [id, mime], ""