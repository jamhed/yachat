define ["pi/Pi", "/js/bullet.js", "Util"], (Pi, Bullet, Util) -> class Bullet extends Pi

   attr: -> super.concat ["uri"]

   init: ->

      @uri = @a.uri || "ws://" + window.location.hostname + ":" + window.location.port + "/main/ws/"
      
      @bullet = $.bullet @uri, disableWebSocket: false, disableEventSource: true, disableXHRPolling: true

      @bullet.onopen = () =>
         console.log "conn()"
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
            @conv_status "join", convId
         else
            @conv_status "part"
  
      # conversation events (join, part)

      @sub "#bullet@conv/new", (e, args) =>
         [ status, convId ] = args
         @set_conv_id convId
         @conv_status "join", convId
         
      @sub "#bullet@conv/join", (e, args) =>
         [ status, convId ] = args
         @set_conv_id convId
         @conv_status "join", convId

      @sub "#bullet@conv/leave", (e, args) =>
         [ status, convId ] = args
         @set_conv_id null
         @conv_status "part", convId

   # utility functions

   check_user_id: ->
      userId = parseInt @globalGet "user_id"
      if userId
         @send "user", userId,
 
   user_status: (status, userRec) ->
      console.log "user status:", status
      @_user_status = status
      @event "user/status/#{status}", userRec

   conv_status: (status, convId) ->
      @_conv_status = status
      console.log "conv/status/#{status}", convId
      @event "conv/status/#{status}", convId

   set_conv_id: (convId) ->
      @globalSet "conv_id", convId
      @conv_id = convId

   set_user_id: (userId) ->
      @globalSet "user_id", userId
      @user_id = userId

   error: (m...) -> 
      @rt.append "dialog/error", text: m 
      
   # methods

   send: (msg...) ->
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
         @send "conv/new", @user_id

   leave_conv: ->
      @send "conv/leave", @user_id, @convId

   login: (a...) ->
      h = (new Util()).list2hash a
      @send "user/login", h.email, h.password

   logout: ->
      @send "user/logout"
      @set_user_id null
      @user_status "not_logged"

   register_email: (a...) ->
      h = (new Util()).list2hash a
      @send "user/register", @user_id, h.email, h.password, h.username, h.gender

   send_msg: (msg) -> @send "msg/conv", @user_id, @convId, msg

