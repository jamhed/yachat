define ["pi/Pi"], (Pi) -> class Dumper extends Pi

   attr: -> super.concat ["row"]

   init: ->
      @sub "#bullet@new_msg", (e, args) =>
         [convId, senderId, response] = args
         row = $("<div>").addClass @a.row
         row.html response
         @e.prepend row
