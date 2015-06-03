define ["pi/Pi"], (Pi) -> class Profile extends Pi

   init: ->
      @wait_ajax_done () =>
         @rpc "#bullet@user_info"

   update: ->
