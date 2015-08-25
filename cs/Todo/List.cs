define ["Nsend", "Cmon", "pi/lib/jquery-ui"], (Pi, Cmon, UI) -> class TodoList extends Pi

	attr: -> super.concat ["default", "id", "inline"]

	init: ->
		super
		if @a.inline
			@e.selectmenu
				change: (ev, ui) => @onChange(ev, ui)
		@query()

	onChange: (ev, ui) ->
		@e.attr "default", ui.item.value
		@a.default = ui.item.value
		value = if parseInt(ui.item.value) then parseInt(ui.item.value) else ui.item.value
		@nsend ["todo/move_to", Cmon.sid(), parseInt(@a.id), value], => @query()

	draw: (List) ->
		@e.empty()
		List.push id: "delete", name: "Delete"
		List.push id: "keep", name: "Keep"
		for T in List
			do (T) =>
				sel = if "#{T.id}" == @a.default then "selected" else ""
				@e.append "<option #{sel} value=#{T.id}>#{T.name}"
		if @a.inline
			@e.selectmenu "refresh"

	query: ->
		@nsend ["todo/list", [Cmon.sid()]], (List) => @draw(List)