define ["Nsend", "Cmon"], (Pi, Cmon) -> class ConvText extends Pi

   draw: (avatarList, stamp) ->
      @clear()
      [id,type] = avatarList[0]
      im = $("<img>").attr("src", "/store/avatar/#{id}?#{stamp}").addClass "img-responsive" 
      @e.append im

   query: -> @nsend ["user/avatar", Cmon.sid()], (status, avatarList) => @draw avatarList, ""

   init: ->
      @sub "#bullet@sys/avatar/upload", (e, avatarId) => @draw [[avatarId, "image"]], Date.now()

      @wait_ajax_done => @query()
