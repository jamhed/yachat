define ["Nsend"], (Pi) -> class LoginInit extends Pi

   init: ->
      @wait_ajax_done =>
         @rpc "#bullet@check_user_id"
