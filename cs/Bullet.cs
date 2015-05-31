define ["pi/Pi", "/js/bullet.js"], (Pi, Bullet) -> class Bullet extends Pi

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
         data = JSON.parse e.data
         [ msg, args... ] = data
         @event msg, args

      @bullet.onheartbeat = =>

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
            @error "Cannot confirm UID, request New", @user_id
            @send "user/new"
         else
            @event "login", @user_id

      @sub "#bullet@conv/new", (e, args) =>
         [ status, convId ] = args
         @convId = convId
         
      @sub "#bullet@conv/join", (e, args) =>
         [ status, convId ] = args
         @convId = convId
 
      console.log @bullet.transport()

   get_user_id: ->
      @user_id = parseInt @globalGet "user_id" 
      if ! @user_id
         @send "user/new"
      else
         @send "user", @user_id
         
   send: (msg...) ->
      @bullet.send JSON.stringify msg  

   new_conv: ->
      chatId = parseInt $("#chatId").val()
      if chatId
         @send "conv/join", @user_id, chatId
      else
         @send "conv/new", @user_id

   send_msg: (msg) ->
      console.log "send_msg", msg
      @send "msg/conv", @user_id, @convId, msg

   error: (m...) -> console.log m
