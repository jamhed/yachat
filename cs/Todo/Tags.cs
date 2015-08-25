define ["Nsend", "Cmon"], (Pi, Cmon) -> class TodoTags extends Pi

	attr: -> super.concat ["template", "lists"]

	init: ->
		super
		@wait_ajax_done => @sub "#{@a.lists}@update", => @query()
		@query()

	set: (data) ->
		@nsend ["todo/tag/set", [Cmon.sid()], data.tag], => 
			@rpc "#{@a.lists}@self", [], (b) => b.query()
			@query()

	draw: (CurrentTag, List) ->
		@e.empty()
		@e.append @tmpl @a.template, tag: L, current: CurrentTag == L for L in List
		@process()

	query: ->
		@nsend ["todo/tags", [Cmon.sid()] ], ([CurrentTag], List) => @draw(CurrentTag, List)