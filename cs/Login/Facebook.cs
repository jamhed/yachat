define ["Nsend"], (aPi) -> class LoginFacebook extends aPi

   init: ->
      @wait_ajax_done =>
         @ncall (bo) =>
            # if bo.fb_status == "connected"
            @e.css display: "inline-block", "white-space": "nowrap"
