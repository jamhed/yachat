define ["Nsend", "Cmon", "Util"], (P, Cmon, Util) -> class ProfileAttr extends P

	attr: -> super.concat ["template"]

	init: ->
		super
		@bsub "sys/avatar/change", (e, avatarId) => @query()
		@wait_ajax_done => @query()

	draw: (List) ->
		@e.empty()
		@e.append @tmpl @a.template, Attr for Attr in List
		@process()

	edit: (Data) ->
		@nsend ["user/attr/get", Cmon.sid(), Data.name], (Attr) => 
		@append "attr/dialog", Attr

	add_dialog: -> @append "attr/dialog"

	delete: (Data) ->
		@nsend ["user/attr/del", Cmon.sid(), Data.name], => @query()

	update: (AttrList) ->
		@debug AttrList
		Attr = Util.list2hash AttrList
		@nsend ["user/attr/set", Cmon.sid(), Attr.name, Attr.value], () => @query()

	query: ->
		@nsend ["user/attr/list", Cmon.sid()], (List) => @draw List