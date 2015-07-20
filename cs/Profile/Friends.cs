define ["Nsend", "Cmon"], (Pi, Cmon) -> class ProfileFriends extends Pi

   attr: -> super.concat ["friend"]

   init: ->
      super
      @friend = @tmpl @a.friend
      @wait_ajax_done () => @query()
   
   draw: (List) ->
      @e.empty()
      @e.append @friend friend for friend in List
      @process()

   query: ->
      @nsend ["user/get/friends", Cmon.sid()], (List) => @draw List

   remove: (args) ->
      @nsend ["user/del/friend", Cmon.sid(), args.id], () => @query()

   search_dialog: ->
      @append "dialog/search"

   search: (Args) -> @nsend ["user/lookup", Cmon.sid(), Args.value], (Status, User) => @add(User)

   add: (User) ->
      if User
         @nsend ["user/add/friend", Cmon.sid(), User.id], () => @query()
