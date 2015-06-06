define ["Nsend", "Cmon"], (Pi, Cmon) -> class Profile extends Pi

   init: ->
      @wait_ajax_done () => @query()
   
   draw: ([userId, userName, email]) ->
      $("#username", @e).val(userName)
      $("#email", @e).val(email)

   query: ->
      @nsend ["user/info", Cmon.user_id()], (status, userInfo) => @draw userInfo

   # Uid, Email, Password, FirstName, LastName, UserName, Gender]
   update: (l...)  ->
      h = Cmon.list2hash l
      @nsend ["user/register",  Cmon.user_id(), h.email, h.password, null, null, h.username, null], (status, a) =>
         if status == "ok"
            @info "Profile was updated."
            @rpc "#bullet@pub_event", ["user/status/registered", Cmon.user_id(), h.username, h.email]
         else
            @error "Profile wasn't updated."
