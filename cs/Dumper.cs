define ["pi/Pi"], (Pi) -> class Dumper extends Pi

   attr: -> super.concat ["stamp", "text"]

   init: ->
      @sub "#bullet@new_msg", (e, args) =>
         [convId, [id,name,email], [stamp,text]] = args
         text_div = $("<div>").addClass(@a.text).html(text)
         stamp_div = $("<div>").addClass(@a.stamp).html(stamp)
         @e.prepend text_div
         @e.prepend stamp_div
