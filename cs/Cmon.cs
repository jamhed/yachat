define [], () -> class Cmon

   @get: (k) ->
      try
         localStorage[k]
      catch e
         console.log "localstorage fail."
      
   @set: (k,v) -> 
      try
         localStorage[k] = v
      catch e
         console.log "localstorage fail set."

   @set_sid: (sid) -> @set("session_id", sid)

   @set_conv_id: (conv_id) -> @set("conv_id", conv_id)

   @sid: -> parseInt @get("session_id")

   @conv_id: -> parseInt @get("conv_id")

   @displayName: (id,name,email) -> if name then name else if email then email else id

   @displayNameA: (user) ->
      [id,name,email] = user
      @displayName id,name,email

   @list2hash: (list) ->
      ret = {}
      for elem in list
         ret[elem.name] = elem.value
      return ret
