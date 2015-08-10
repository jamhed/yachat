define ["Nsend", "Cmon"], (Pi, Cmon) -> class TodoList extends Pi

	attr: -> super.concat ["default"]

	init: ->
		@query()

	draw: (List) ->
		List.push id: "delete", name: "Delete"
		List.push id: "keep", name: "Keep"
		for T in List
			do (T) =>
				sel = if "#{T.id}" == @a.default then "selected" else ""
				@e.append "<option #{sel} value=#{T.id}>#{T.name}"

	query: ->
		@nsend ["todo/list", Cmon.sid()], (List) => @draw(List)