define ["pi/Pi"], (Pi) -> class Conv extends Pi

   attr: -> super.concat ["row"]

   init: ->
      @sub "#bullet@conv/new", (e, args) =>
         [response, convId] = args
         @e.val convId
