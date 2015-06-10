define ["pi/Pi", "Cmon", "pi/lib/upload"], (aPi, Cmon, Upload) -> class Upload extends aPi 

   init: ->
      @e.fileupload
         formData: [ {name: "sid", value: Cmon.sid()} ]
