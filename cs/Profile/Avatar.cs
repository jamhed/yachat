define ["Nsend", "Cmon"], (Pi, Cmon) -> class ConvText extends Pi

   draw: (avatarList) ->
      @clear()
      [id,type] = avatarList[0]
      im = $("<img>").attr("src", "/store/avatar/#{id}").addClass "img-responsive" 
      @e.append im

   query: -> @nsend ["user/avatar", Cmon.sid()], (status, avatarList) => @draw avatarList

   init: ->
      @wait_ajax_done => @query()
