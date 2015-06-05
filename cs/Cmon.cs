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

   @set_user_id: (user_id) -> @set("user_id", user_id)

   @set_conv_id: (conv_id) -> @set("conv_id", conv_id)

   @user_id: -> parseInt @get("user_id")

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
