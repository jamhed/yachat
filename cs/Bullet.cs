define ["Nsend", "/js/bullet.js", "Cmon"], (Pi, Bullet, Cmon) -> class Bullet extends Pi
# define ["Nsend", "/js/bullet.js", "Cmon", "//connect.facebook.net/en_US/sdk.js"], (Pi, Bullet, Cmon) -> class Bullet extends Pi

   seq: 0
   cb_nsend: null

   fb_status: null
   fb_token: null
   fb_fbid: null

   _user_status: null

   attr: -> super.concat ["uri", "fb_app"]

   init: ->

      if FB?
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
      
      @handler "conv_msg", (e, [convId, [stamp, [msg, user]]]) =>
         # p2p chat
         if msg == "p2p"
            Cmon.set_conv_id convId
            @conv_status "join", convId
         if msg == "part"
            @debug "PART", convId
         if msg == "join"
            @debug "JOIN", convId

      @handler "sys_msg", (e, [[msg,id]]) =>
         @debug "sys msg", msg
         @event "sys/#{msg}", id


      # user events (logged, registered, not_logged)

      @handler "user/new", (e, args) =>
         
         [ status, sessionId, userId ] = args

         if status == "ok"
            Cmon.set_sid sessionId
            @user_status "anonymous", [userId]
         else
            @user_status ="not_logged"
            @error "Server protocol"

      @handler "user/get", (e, args) =>
         [ status, sessionId, userInfo ] = args
         
         if status == "fail"
            Cmon.set_sid null
            return @user_status "not_logged"

         Cmon.set_sid sessionId
         if userInfo[1]
            @user_status "registered", userInfo
         else
            @user_status "anonymous", userInfo

      @handler "user/fb", (e, args) =>
         [ status, sessionId, userInfo ] = args
         if status == "ok"
            Cmon.set_sid sessionId
            @user_status "registered", userInfo
         else
            Cmon.set_sid null
            @error "Account is not found, please re-register"
            return @user_status "not_logged"
      
      @handler "user/login", (e, args) =>
         [ status, sessionId, userInfo ] = args
         if status == "ok"
            Cmon.set_sid sessionId
            @user_status "registered", userInfo
         else
            Cmon.set_sid null
            @error "Login or password error: " + userId 
            return @user_status "not_logged"

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
         if status == "ok"
            Cmon.set_conv_id convId
            @conv_status "join", convId
         else
            @conv_status "part"
            @error "Error join conversation!"

      @handler "user/p2p", (e, args) =>
         [ status, convId ] = args
         if status != "ok"
            @error "Error making p2p!"

      @handler "conv/leave", (e, args) =>
         [ status, convId ] = args
         @conv_status "part", convId
         Cmon.set_conv_id null

      @handler "user/logout", (e, args) =>
         @conv_status "part", null
         Cmon.set_conv_id null
         window.location = "#"

   # utility functions

   check_user_id: ->
      sessionId = Cmon.sid()
      if sessionId
         @send "user/get", sessionId,
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
         @send "conv/join", Cmon.sid(), chatId
      else
         if Cmon.sid()
            @send "conv/new", Cmon.sid()

   leave_conv: ->
      @send "conv/leave", Cmon.sid(), Cmon.conv_id()

   pub_event: (ev, args...) -> @event ev, args

   anonymous: ->
      @send "user/new", []

   login: (a...) ->
      h = Cmon.list2hash a
      @send "user/login", [h.email, h.password]

   logout: ->
      @send "user/logout", []
      Cmon.set_sid null
      @user_status "not_logged"
      window.location="#"

   send_msg: (msg) -> @send "msg/conv", Cmon.sid(), Cmon.conv_id(), msg

   init_p2p: (args) ->
      peerId = args.user_id
      if peerId?
         @send "user/p2p", Cmon.sid(), peerId

   invite: (a...) ->
      h = Cmon.list2hash a
      @nsend ["user/email", Cmon.sid(), h.email], (status, user) =>
         if status == "ok"
            [id,name,email] = user
            @nsend ["conv/join", id, Cmon.conv_id()], (status, convId) =>
               @event "conv/status/invite"
         else
            @error "Can't find user by email."

   dialog_invite: ->
      @rt.append "dialog/invite"

   register_facebook: ->
      FB.login ((r) => @handle_fb_register(r)), scope: "public_profile,email"

   fb_login: ->
      if @fb_status == "connected"
         @send "user/fb", [@fb_id]
      else
         @error "Connect profile to facebook first!"

   handle_fb_auth: (r) ->
      if r.status == "connected"
         @fb_status = "connected"
         @fb_token = r.authResponse.accessToken
         @fb_id = r.authResponse.userID

   handle_fb_register: (r) ->
      if r.status == "connected"
         @handle_fb_auth r
         FB.api "/me", (r) =>
            @nsend ["user/update", Cmon.sid(),
               "facebook_id", r.id,
               "email", r.email,
               "firstname", r.first_name,
               "lastname", r.last_name,
               "username", r.name,
               "gender", r.gender], (r) => @handle_fb_register_ok(r)
      else
         @error "Facebook status #{r.status}"
 
   handle_fb_register_ok: (status) ->
      if status == "ok"
         @send "user/profile", Cmon.sid()
      else
         @error "Error updating profile."


   set_user_attr: ->
      @send "user/attr/set", Cmon.sid(), test: "value", more: 123

   get_user_attr: ->
      @send "user/attr/get", Cmon.sid()
