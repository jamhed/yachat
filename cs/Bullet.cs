define ["Nsend", "/js/bullet.js", "Cmon", "//connect.facebook.net/en_US/sdk.js"], (Pi, Bullet, Cmon) -> class Bullet extends Pi

   seq: 0
   cb_nsend: null

   fb_status: null
   fb_token: null
   fb_fbid: null

   _user_status: null

   attr: -> super.concat ["uri", "fb_app"]

   init: ->

      FB.init
         appId: @a.fb_app
         xfbml: true
         version: "v2.3"
      
      FB.getLoginStatus (r) => @handle_fb_auth r

      @cb_nsend = {}

      @uri = @a.uri || "ws://" + window.location.hostname + ":" + window.location.port + "/main/ws/"
      
      @bullet = $.bullet @uri, disableWebSocket: false, disableEventSource: true, disableXHRPolling: true

      @bullet.onopen = () =>
         @debug "conn()"
         @event "conn/open"
     
      @bullet.ondisconnect = =>
         @user_status "not_logged"
         @event "conn/close"
      
      @bullet.onclose = =>
         @user_status "not_logged"
         @event "conn/close"

      @bullet.onmessage = (e) =>
         if e.data != "ping"
            data = JSON.parse e.data
            [ msg, args... ] = data
            @event msg, args

      @bullet.onheartbeat = => @bullet.send "ping"

      # events handlers

      @handler "nmsg", (e, data) =>
         [seq, [msg, args...]] = data
         @debug "NMSG:", seq, msg, args
         if @cb_nsend[seq]
            if @cb_nsend[seq].msg == msg
               @cb_nsend[seq].fn args...
            else
               @debug "NMSG:", "unmatched message for seq", seq, msg, @cb_nsend[seq].msg, args
            delete @cb_nsend[seq]
         else
            @error "no callback for seq", seq
      
      @handler "sys_msg", (e, args) =>
         [cid,user,[stamp,ev]] = args 

      # user events (logged, registered, not_logged)

      @handler "user/new", (e, args) =>
         
         [ status, userId ] = args

         if status == "new"
            Cmon.set_user_id userId
            @user_status "anonymous", [userId]
         else
            @user_status ="not_logged"
            @error "Server protocol"

      @handler "user", (e, args) =>
         [ status, userId ] = args
         
         if status == "fail"
            @send "user/new"
         else
            @send "user/info", userId

      @handler "user/info", (e, args) =>
         [ status, [userId, name, email] ] = args
         
         Cmon.set_user_id userId
        
         if email
            @user_status "registered", [userId, name, email]
         else
            @user_status "anonymous", [userId]

      @handler "user/fb", (e, args) =>
         [ status, [userId, name, email] ] = args
         
         if userId
            Cmon.set_user_id userId
            @user_status "registered", [userId, name, email]
      
      @handler "user/register", (e, args) =>
         [status, userId] = args
         if status == "ok"
            @send "user/info", userId
            window.location = "#"
         else
            @error "Register Error", userId

      @handler "user/login", (e, args) =>
         [ status, userId ] = args
         if status == "ok"
            @send "user/info", userId
         else
            @error "Login or password error: " + userId 

      # conversation events (join, part)

      @handler "conv/new", (e, args) =>
         [ status, convId ] = args
         if status == "ok"
            Cmon.set_conv_id convId
            @conv_status "join", convId
         else
            Cmon.set_conv_id null
            @conv_status "part"
         
      @handler "conv/join", (e, args) =>
         [ status, convId ] = args
         Cmon.set_conv_id convId
         @conv_status "join", convId

      @handler "conv/leave", (e, args) =>
         [ status, convId ] = args
         @conv_status "part", convId
         Cmon.set_conv_id null

      @handler "user/logout", (e, args) =>
         @conv_status "part", null
         Cmon.set_conv_id null

   # utility functions

   check_user_id: ->
      userId = Cmon.user_id()
      if userId
         @send "user", userId,
      else
         @user_status "not_logged"
 
   user_status: (status, userRec) ->
      @debug "user status:", status
      @_user_status = status
      @event "user/status/#{status}", userRec

   conv_status: (status, convId) ->
      @_conv_status = status
      @debug "conv status:", status, convId
      @event "conv/status/#{status}", convId

   error: (m...) -> 
      @rt.append "dialog/error", text: m.join(" ")

   event: (e,args) =>
      @debug "EVENT", e, args # [].concat args...
      super e, args
      
   # methods

   send: (msg...) ->
      @debug "MSG", msg
      @bullet.send JSON.stringify msg  

   # [msg, arg1, ..]
   nsend: (msg, callback) ->
      @seq = @seq + 1
      @debug "N-MSG", @seq, msg
      @cb_nsend[@seq] = fn: callback, msg: msg[0]
      @bullet.send JSON.stringify ["nmsg", @seq, msg]

   # public methods, called as @rpc

   join_conv: (conv) ->
      chatId = if conv.conv then conv.conv else parseInt $("#chatId").val()
      if chatId
         @send "conv/join", Cmon.user_id(), chatId
      else
         if Cmon.user_id()
            @send "conv/new", Cmon.user_id()

   leave_conv: ->
      @send "conv/leave", Cmon.user_id(), Cmon.conv_id()

   pub_event: (ev, args...) -> @event ev, args

   anonymous: ->
      @send "user/new"

   login: (a...) ->
      h = Cmon.list2hash a
      @send "user/login", h.email, h.password

   logout: ->
      @send "user/logout"
      Cmon.set_user_id null
      @user_status "not_logged"
      window.location="#"

   register_email: (a...) ->
      h = Cmon.list2hash a
      @send "user/register", Cmon.user_id(), h.email, h.password, h.firstname, h.lastname, h.username, h.gender

   send_msg: (msg) -> @send "msg/conv", Cmon.user_id(), Cmon.conv_id(), msg

   invite: (a...) ->
      h = Cmon.list2hash a
      @nsend ["user/email", h.email], (status, user) =>
         if status == "ok"
            [id,name,email] = user
            @nsend ["conv/join", id, Cmon.conv_id()], (status, convId) =>
               @event "conv/status/invite"
         else
            @error "Can't find user by email."

   dialog_invite: ->
      @rt.append "dialog/invite"

   register_facebook: ->
      if @fb_status != "connected"
         FB.login ((r) => @handle_fb_register r), scope: "public_profile,email"

   fb_login: ->
      if @fb_status == "connected"
         @send "user/fb", @fb_id
      else
         @error "Connect profile to facebook first!"

   handle_fb_auth: (r) ->
      if r.status == "connected"
         @fb_status = "connected"
         @fb_token = r.authResponse.accessToken
         @fb_id = r.authResponse.userID

   handle_fb_regsiter: (r) ->
      if r.status == "connected"
         @handle_fb_auth r
         FB.api "/me", (r) =>
            @nsend ["user/facebook",  Cmon.user_id(), r.id, r.email, r.first_name, r.last_name, r.name, r.gender]
      else
         @error "Facebook status #{r.status}"
 
