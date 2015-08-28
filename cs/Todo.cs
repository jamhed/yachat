define ["Nsend", "Cmon", "Util"], (Pi, Cmon, Util) -> class Todo extends Pi

	attr: -> super.concat ["todo", "item"]

	init: ->
		super
		@todo = @tmpl @a.todo
		@item = @tmpl @a.item      
		@wait_ajax_done () => @query()
	
	draw: (List) ->
		@e.empty()
		for T in List
			do (T) =>
				@e.append @todo T
				items = $("##{T.id}.items", @e)
				for I in T.items
					do (I) =>
						I.list_id = T.id
						items.append @item I 
		@process()
		$("#entry-field").focus()

	click_item: (Data) ->
		@nsend ["todo/click", Cmon.sid(), Data.listId, Data.id], => @query()

	delete: (Data) -> @nsend ["todo/del", Cmon.sid(), Data.id], =>
		@notify()
		@query()

	edit: (data) ->
		@nsend ["todo/load", Cmon.sid(), data.id], ([List]) => @append "todo/dialog", List

	add_dialog: ->
		@nsend ["todo/tag/current", Cmon.sid()], (Tag) => @append "todo/dialog", current_tag: Tag

	update: (Todo, Params) ->
		h = Util.list2hash Todo
		Todo.push name: "id", value: Params.id
		Todo.push name: "move_to", value: if parseInt(h.move_to) then parseInt(h.move_to) else h.move_to
		@nsend ["todo/update", Cmon.sid(), Todo], () =>
			@notify Todo
			@query()
	
	export: ->
		@nsend ["todo/export", Cmon.sid()], (Data) =>
			blob = new Blob([Data], type: "text/plain")
			url = window.URL.createObjectURL(blob)
			a = document.createElement("a")
			document.body.appendChild(a)
			a.style = "display: none"
			a.href = url
			a.download = "todo.txt"
			a.click()
			window.URL.revokeObjectURL(url)

	notify: (Msg = "") -> @event "update", Msg

	query: ->
		@nsend ["todo/get", Cmon.sid()], (List) => @draw List