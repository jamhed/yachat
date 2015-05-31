define ["pi/Pi", "/js/bullet.js"], (Pi, Bullet) -> class Bullet extends Pi

   attr: -> super.concat ["uri"]

   init: ->
      @uri = @a.uri || "ws://" + window.location.hostname + ":" + window.location.port + "/main/ws/"
      
      @bullet = $.bullet @uri

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

   get_user_id: ->
      @user_id = @globalGet "user_id" 
      if ! @user_id
         @send "user/new"
      else
         @send "user", @user_id
         
   send: (msg...) ->
      @bullet.send JSON.stringify msg  

   test: ->
      @send "test", "msg"

   error: (m...) -> console.log m
