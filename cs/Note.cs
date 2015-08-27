define ["Nsend", "Cmon", "Util"], (Pi, Cmon, Util) -> class Note extends Pi

	attr: -> super.concat ["template"]

	init: ->
		super
		@node = @tmpl @a.template
		@wait_ajax_done () => @query()

	draw: (List) ->
		@clear()
		@e.append @node L for L in List
		@process()

	query: -> @nsend ["note/get", Cmon.sid()], (List) => @draw List

	# [{name:, value:}, ...]
	add: ([data]) ->
		@nsend ["note/add", Cmon.sid(), data.value], (Status) => @handle_status(Status)

	clear_input: -> $(".el").val("")

	handle_status: (Status) ->
		if Status == "ok"
			@clear_input()
			@query()