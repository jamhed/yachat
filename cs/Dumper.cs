define ["pi/Pi"], (Pi) -> class Dumper extends Pi

   attr: -> super.concat ["stamp", "text"]

   init: ->
      @sub "#bullet@new_msg", (e, args) =>
         [convId, [id,name,email], [stamp,text]] = args
         display = if name != "undefined" then name else if email != "undefined" then email else id
         text_div = $("<div>").addClass(@a.text).html(text)
         stamp_div = $("<div>").addClass(@a.stamp).html(stamp + "&nbsp;&nbsp;" + display + ":")
         holder = $("<div>").addClass("row")
         holder.prepend text_div
         holder.prepend stamp_div
         @e.prepend holder
