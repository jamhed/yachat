define ["Nsend", "Cmon"], (Pi, Cmon) -> class Profile extends Pi

   init: ->
      @sub "#bullet@user/profile", (ev, [status, userInfo]) => @draw userInfo
      @wait_ajax_done () => @query()
   
   draw: (u) ->
      $("#username", @e).val(u.username)
      $("#email", @e).val(u.email)

   query: ->
      @nsend ["user/get", Cmon.sid()], (status, sessionId, userInfo) =>
         @debug status, userInfo
         @draw userInfo

   update: (l...)  ->
      h = Cmon.list2hash l
      @nsend ["user/update", Cmon.sid(), h], (status, a) =>
         if status == "ok"
            @info "Profile was updated."
            @rpc "#bullet@pub_event", ["user/status/registered", Cmon.sid(), h.username, h.email]
         else
            @error "Profile wasn't updated."
