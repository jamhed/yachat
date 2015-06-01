define ["pi/Pi", "/js/bullet.js", "Util"], (Pi, Bullet, Util) -> class Bullet extends Pi

   attr: -> super.concat ["uri"]

   init: ->
      @uri = @a.uri || "ws://" + window.location.hostname + ":" + window.location.port + "/main/ws/"
      
      @bullet = $.bullet @uri, disableWebSocket: false

      @bullet.onopen = () =>
         console.log "conn()"
         @get_user_id()
      
      @bullet.ondisconnect = =>
      
      @bullet.onclose = =>

      @bullet.onmessage = (e) =>
         if e.data != "ping"
            data = JSON.parse e.data
            [ msg, args... ] = data
            @event msg, args

      @bullet.onheartbeat = => @bullet.send "ping"

      @sub "#bullet@user/new", (e, args) =>
         
         [ status, number ] = args

         if status == "new"
            @user_id = number
            @globalSet "user_id", @user_id
         else
            @error "Cannot Get UID"

      @sub "#bullet@user", (e, args) =>
         [ status ] = args
         
         if status == "fail"
            @error "Cannot confirm UID, request New"
            @send "user/new"
         else
            @send "user/info", @user_id
            @event "login", @user_id

      @sub "#bullet@conv/new", (e, args) =>
         [ status, convId ] = args
         @convId = convId
         
      @sub "#bullet@conv/join", (e, args) =>
         [ status, convId ] = args
         @convId = convId

      @sub "#bullet@user/info", (e, args) =>
         [ status, [userId, name, email] ] = args
         if !email
            @event "anonymous", @user_id

      @sub "#bullet@user/login", (e, args) =>
         [ status, user_id ] = args
         if status == "ok"
            @user_id = user_id
            @send "user/info", @user_id
            @event "login", @user_id
         else
            @error "Login or password error" # user_id = cause
   
      console.log @bullet.transport()

   get_user_id: ->
      @user_id = parseInt @globalGet "user_id" 
      if ! @user_id
         @send "user/new"
      else
         @send "user", @user_id
         
   send: (msg...) ->
      @bullet.send JSON.stringify msg  

   # Public, called as @rpc

   new_conv: ->
      chatId = parseInt $("#chatId").val()
      if chatId
         @send "conv/join", @user_id, chatId
      else
         @send "conv/new", @user_id

   login: (a...) ->
      h = (new Util()).list2hash a
      @send "user/login", h.email, h.password

   register_email: (a...) ->
      h = (new Util()).list2hash a
      @send "user/register", @user_id, h.email, h.password, h.username, h.gender

   send_msg: (msg) ->
      console.log "send_msg", msg
      @send "msg/conv", @user_id, @convId, msg

   error: (m...) -> 
      @rt.append "dialog/error", text: m
