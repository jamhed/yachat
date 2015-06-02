define ["pi/Pi", "/js/bullet.js", "Util"], (Pi, Bullet, Util) -> class Bullet extends Pi

   attr: -> super.concat ["uri"]

   init: ->

      @uri = @a.uri || "ws://" + window.location.hostname + ":" + window.location.port + "/main/ws/"
      
      @bullet = $.bullet @uri, disableWebSocket: false, disableEventSource: true, disableXHRPolling: true

      @bullet.onopen = () =>
         @log "conn()"
         @wait_ajax_done () =>
            @log "AJAX DONE"
            @user_status "not_logged"
            @check_user_id()
      
      @bullet.ondisconnect = =>
         @user_status "not_logged"
      
      @bullet.onclose = =>
         @user_status "not_logged"

      @bullet.onmessage = (e) =>
         if e.data != "ping"
            data = JSON.parse e.data
            [ msg, args... ] = data
            @event msg, args

      @bullet.onheartbeat = => @bullet.send "ping"

      # events handlers

      # user events (logged, registered, not_logged)

      @sub "#bullet@user/new", (e, args) =>
         
         [ status, userId ] = args

         if status == "new"
            @set_user_id userId
            @user_status "anonymous"
         else
            @user_status ="not_logged"
            @error "Server protocol"

      @sub "#bullet@user", (e, args) =>
         [ status, userId ] = args
         
         if status == "fail"
            @send "user/new"
         else
            @send "user/info", userId

      @sub "#bullet@user/info", (e, args) =>
         [ status, [userId, name, email] ] = args
         
         @set_user_id userId
        
         if email == "undefined"
            @user_status "anonymous", [userId]
         else
            @user_status "registered", [userId, name, email]
         
      @sub "#bullet@user/register", (e, args) =>
         [status, userId] = args
         if status == "ok"
            @send "user/info", userId
            window.location = "#"
         else
            @error "Register Error", userId

      @sub "#bullet@user/login", (e, args) =>
         [ status, userId ] = args
         if status == "ok"
            @send "user/info", userId
         else
            @error "Login or password error: " + cause # userId = cause

      @sub "#bullet@user/conv_list", (e, args) =>
         [ status, convList ] = args
         convId = parseInt @globalGet "conv_id"
         if convId in convList
            @set_conv_id convId
            @conv_status "join", convId
         else
            @conv_status "part"
  
      # conversation events (join, part)

      @sub "#bullet@conv/new", (e, args) =>
         [ status, convId ] = args
         if status == "ok"
            @set_conv_id convId
            @conv_status "join", convId
         else
            @set_conv_id null
            @conv_status "part"
         
      @sub "#bullet@conv/join", (e, args) =>
         [ status, convId ] = args
         @set_conv_id convId
         @conv_status "join", convId

      @sub "#bullet@conv/leave", (e, args) =>
         [ status, convId ] = args
         @conv_status "part", convId
         @set_conv_id null

   # utility functions

   check_user_id: ->
      userId = parseInt @globalGet "user_id"
      if userId
         @send "user", userId,
 
   user_status: (status, userRec) ->
      @log "user status:", status
      @_user_status = status
      @event "user/status/#{status}", userRec

   conv_status: (status, convId) ->
      @_conv_status = status
      @log "conv status:", status, convId
      @event "conv/status/#{status}", convId

   set_conv_id: (convId) ->
      @globalSet "conv_id", convId
      @conv_id = convId

   set_user_id: (userId) ->
      @globalSet "user_id", userId
      @user_id = userId

   error: (m...) -> 
      @rt.append "dialog/error", text: m 

   event: (e,args) =>
      @log "EVENT", e, args
      super e, args

   log: (m...) ->
      console.log m
      $("#log").append [m], "\n"
      
   # methods

   send: (msg...) ->
      @log "MSG", msg
      @bullet.send JSON.stringify msg  

   # public methods, called as @rpc
   
   query_convs: ->
      userId = parseInt @globalGet "user_id"
      @send "user/conv_list", userId
        
   join_conv: ->
      chatId = parseInt $("#chatId").val()
      if chatId
         @send "conv/join", @user_id, chatId
      else
         if @user_id
            @send "conv/new", @user_id

   leave_conv: ->
      @send "conv/leave", @user_id, @conv_id

   conv_history: ->
      @send "conv/history", @conv_id

   login: (a...) ->
      h = (new Util()).list2hash a
      @send "user/login", h.email, h.password

   anonymous: ->
      @send "user/new"

   logout: ->
      @send "user/logout"
      @set_user_id null
      @user_status "not_logged"

   register_email: (a...) ->
      h = (new Util()).list2hash a
      @send "user/register", @user_id, h.email, h.password, h.username, h.gender

   send_msg: (msg) -> @send "msg/conv", @user_id, @conv_id, msg

