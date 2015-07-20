define ["Nsend", "Cmon"], (Pi, Cmon) -> class Online extends Pi

   attr: -> super.concat ["view"]

   init: ->
      super
      @wait_ajax_done () => @query()
   
   draw: (userList) ->
      @clear()
      tmpl = @tmpl @a.view
      @e.append tmpl {id: userInfo.id, display: Cmon.displayNameA(userInfo) } for userInfo in userList
      @process @e

   query: ->
      @nsend ["user/online", Cmon.sid()], (userList) => @draw userList