define ["pi/Pi"], (Pi) -> class Dumper extends Pi

   attr: -> super.concat ["stamp", "text"]

   init: ->
      @sub "#bullet@new_msg", (e, args) =>
         [convId, user, msg] = args
         @append user, msg

      @sub "#bullet@conv/history", (e,args) =>
         [status, rows] = args
         for row in rows.reverse()
            [user, msg] = row
            @append user, msg

      @sub "#bullet@conv/status/part", (e,args) =>
         @e.empty()

      @sub "#bullet@conv/status/join", (e, args) =>
         @rpc "#bullet@conv_history"
         @e.empty()

      @wait_ajax_done () =>
         @debug "AJAX DONE"
 
   append: (user, msg) ->
      [id,name,email] = user
      [stamp,text] = msg
      display = if name != "undefined" then name else if email != "undefined" then email else id
      text_div = $("<div>").addClass(@a.text).html(text)
      stamp_div = $("<div>").addClass(@a.stamp).html(stamp + "&nbsp;&nbsp;" + display + ":")
      holder = $("<div>").addClass("row")
      holder.prepend text_div
      holder.prepend stamp_div
      @e.prepend holder


