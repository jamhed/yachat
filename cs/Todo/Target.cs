define ["Nsend", "Cmon", "pi/lib/jquery-ui"], (Pi, Cmon, UI) -> class TodoTarget extends Pi

	attr: -> super.concat ["inline"]

	init: ->
		super
		if @a.inline
			@e.selectmenu
				change: (ev, ui) => @onChange(ev, ui)
		@query()

	onChange: (ev, ui) ->
		value = if parseInt(ui.item.value) then parseInt(ui.item.value) else ui.item.value

	draw: (List, [Default]) ->
		@debug Default
		@e.empty()
		for T in List
			do (T) =>
				sel = if T.id == Default.id then "selected" else ""
				@e.append "<option #{sel} value=#{T.id}>#{T.name}"
		
		if @a.inline
			@e.selectmenu "refresh"

	query: ->
		@nsend ["todo/list", [Cmon.sid()]], (List, Default) => @draw(List, Default)