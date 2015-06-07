define ["Nsend"], (Pi) -> class LoginInit extends Pi

   init: ->
      @wait_ajax_done =>
         return if @e.attr("processed") == "2"
         @rpc "#bullet@check_user_id"
