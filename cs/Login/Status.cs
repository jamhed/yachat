define ["Nsend", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class LoginStatus extends aPi

	attr: -> super.concat ["login", "logout"]

	logged: (user) ->
		@clear()
		tmpl = @tmpl @a.logout
		@e.html tmpl display: Cmon.displayNameA user
		@process @e

	not_logged: ->
		@debug "not_logged"
		@clear()
		@e.html @tmpl @a.login
		@process @e
 
	init: ->
		super
		
		@bsub "user/status", (ev, [status, user]) =>
			if status == "anonymous" or status == "registered"
				@logged user
			else
				@not_logged()
