define ["pi/Pi", "pi/lib/jquery-ui"], (aPi, jqUI) -> class DialogSimple extends aPi

	init: ->
		@e.dialog
			close: (ev, ui) => @e.remove()
			draggable: true
			width: @data.width || 500
			height: @data.height || "auto"
			position: my: "top", at: "top+150"

	close: -> @e.remove()