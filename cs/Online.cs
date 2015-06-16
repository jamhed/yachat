define ["Nsend", "Cmon"], (Pi, Cmon) -> class Online extends Pi

   attr: -> super.concat ["view"]

   init: ->
      @wait_ajax_done () => @query()
   
   draw: (userList) ->
      @clear()
      tmpl = @rt.source @a.view
      @e.append tmpl {id: userInfo.id, display: Cmon.displayNameA(userInfo) } for userInfo in userList
      @rt.pi @e

   query: ->
      @nsend ["user/online", Cmon.sid()], (status, userList) =>
         @debug status, userList
         @draw userList
