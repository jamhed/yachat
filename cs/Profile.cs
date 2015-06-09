define ["Nsend", "Cmon"], (Pi, Cmon) -> class Profile extends Pi

   init: ->
      @sub "#bullet@user/profile", (ev, [status, userInfo]) => @draw userInfo
      @wait_ajax_done () => @query()
   
   draw: ([userId, userName, email]) ->
      $("#username", @e).val(userName)
      $("#email", @e).val(email)

   query: ->
      @nsend ["user/get", Cmon.sid()], (status, sessionId, userInfo) =>
         @debug status, userInfo
         @draw userInfo

   update: (l...)  ->
      h = Cmon.list2hash l
      @nsend ["user/update", Cmon.sid(), "email", h.email, "username", h.username], (status, a) =>
         if status == "ok"
            @info "Profile was updated."
            @rpc "#bullet@pub_event", ["user/status/registered", Cmon.sid(), h.username, h.email]
         else
            @error "Profile wasn't updated."
