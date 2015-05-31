define ["pi/Pi"], (Pi) -> class Dumper extends Pi

   attr: -> super.concat ["row"]

   init: ->
      @sub "#bullet@stub", (e, args) =>
         [response] = args
         row = $("<div>").addClass @a.row
         row.html response
         @e.append row
