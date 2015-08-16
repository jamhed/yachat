define ["Nsend", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class ConvList extends aPi

	attr: -> super.concat ["view"]

	draw: (List) ->
		tmpl = @tmpl @a.view
		
		@clear()
		for conv in List
			Name = if conv.name then conv.name else conv.id
			@e.append tmpl id: conv.id, name: Name
	
		@process @e

	query: -> @nsend ["user/conv_list", Cmon.sid()], (status, List) => @draw List

	autojoin: (List) ->
		storedConvId = Cmon.conv_id()
		seen = 0
		for conv in List
			if storedConvId == conv.id
				@rpc "#bullet@pub_event", ["conv/status/join", conv.id]
				seen = 1
		if ! seen
			@rpc "#bullet@pub_event", ["conv/status/part"]
 
	init: ->
		super
		
		@bsub "conv/update", (ev, args) => @query()
		@bsub "conv/status/join", (ev, args) => @query()
		@bsub "conv/status/part", (ev, args) => @query()
		@bsub "user/status/registered", (ev, args) => @query()
		@bsub "user/status/anonymous", (ev, args) => @query()
		@bsub "user/status/not_logged", (ev, args) => @clear()

		@wait_ajax_done =>
			@nsend ["user/conv_list", Cmon.sid()], (status, List) => @autojoin List

