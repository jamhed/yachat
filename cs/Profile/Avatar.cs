define ["Nsend", "Cmon"], (Pi, Cmon) -> class ConvText extends Pi

   draw: (avatarList) ->
      @clear()
      [id,type] = avatarList[0]
      stamp = Date.now()
      @debug id, stamp
      im = $("<img>").attr("src", "/store/avatar/#{id}?#{stamp}").addClass "img-responsive" 
      @e.append im

   query: -> @nsend ["user/avatar", Cmon.sid()], (status, avatarList) => @draw avatarList

   init: ->
      @sub "#bullet@sys/avatar/upload", (e, avatarId) => @draw [[avatarId, "image"]]

      @wait_ajax_done => @query()
