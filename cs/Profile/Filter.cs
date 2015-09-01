define ["Nsend", "Cmon"], (Pi, Cmon) -> class ProfileFilter extends Pi

	attr: -> super.concat ["template", "el"]

	init: ->
		super
		@user_tmpl = @tmpl @a.template
		@e.keyup (e) =>
			if e.keyCode == 13
				@nsend ["user/search", Cmon.sid(), @e.val()], (List) => @handle_status List

	handle_status: (List) ->
		if List
			el = $ @a.el
			el.empty()
			el.append @user_tmpl U for U in List