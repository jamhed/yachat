define ["Nsend", "Cmon", "Util"], (Pi, Cmon, Util) -> class TodoProject extends Pi

	attr: -> super.concat ["template"]

	init: ->
		super
		@wait_ajax_done () => @query()
	
	draw: (List) ->
		@e.empty()
		@process()

	edit_dialog: (List) ->
		e = @append "todo/project/dialog", List

	delete: (Data) -> @nsend ["todo/project/del", Cmon.sid(), Data.id], => @query()

	edit: (data) ->
		@nsend ["todo/project/get", Cmon.sid(), data.id], ([List]) => @edit_dialog List

	update: (Todo, Params) ->
		h = Util.list2hash Todo
		Todo.push name: "id", value: Params.id
		@nsend ["todo/project/update", Cmon.sid(), Todo], () => @query()
	
	add_dialog: -> @append "todo/project/dialog"

	query: ->
		@nsend ["todo/project/get", Cmon.sid()], (List) => @draw List