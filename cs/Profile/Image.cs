define ["Nsend", "Cmon", "Util"], (P, Cmon, Util) -> class ProfileImage extends P
   
	attr: -> super.concat ["template"]
	init: ->
		super
		@bsub "sys/image/upload", (e, avatarId) => @query()
		@wait_ajax_done => @query()

	draw: (List) ->
		@debug List
		@e.empty()
		for E in List
			do (E) =>
				[Id,Type,Mime] = E
				Src = "/store/#{Type}/#{Id}"
				@e.append @tmpl @a.template, src: Src, id: Id, mime: Mime   
		@process()

	show_dialog: (Data) ->
		Data.src = "/store/avatar/#{Data.id}"
		@append "image/dialog", Data

	set_avatar: (Data) ->
		@nsend ["user/avatar/set", Cmon.sid(), Data.id], ->

	delete: (Data) ->
		@nsend ["user/file/delete", Cmon.sid(), Data.id], => @query()

	query: ->
		@nsend ["user/files", Cmon.sid()], (Status, List) => @draw List