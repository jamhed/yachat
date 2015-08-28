define ["Nsend", "Cmon", "pi/lib/jquery-ui"], (Pi, Cmon, UI) -> class TodoTarget extends Pi

	attr: -> super.concat ["inline", "lists"]

	init: ->
		super
		if @a.inline
			@e.selectmenu
				change: (ev, ui) => @onChange(ev, ui)
		@wait_ajax_done => @sub "#{@a.lists}@update", => @query()
		@query()

	onChange: (ev, ui) ->
		value = if parseInt(ui.item.value) then parseInt(ui.item.value) else ui.item.value
		@nsend ["todo/default", Cmon.sid(), value], () =>

	draw: (List, [Default]) ->
		@e.empty()
		for T in List
			do (T) =>
				sel = if Default && T.id == Default.id then "selected" else ""
				@e.append "<option #{sel} value=#{T.id}>#{T.name}"
		
		if @a.inline && List && List[0]
			@e.selectmenu "refresh"

		if not Default && List[0]
			@nsend ["todo/default", Cmon.sid(), List[0].id], () =>

	query: ->
		@nsend ["todo/list", Cmon.sid()], (List, Default) => @draw(List, Default)