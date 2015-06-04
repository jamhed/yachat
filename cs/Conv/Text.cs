define ["pi/Pi"], (Pi) -> class ConvText extends Pi

   attr: -> super.concat ["stamp", "text"]

   init: ->
      @sub "#bullet@new_msg", (e, args) =>
         [convId, user, msg] = args
         @append user, msg

      @sub "#bullet@conv/history", (e,args) =>
         @empty()
         [status, rows] = args
         for row in rows.reverse()
            [user, msg] = row
            @append user, msg

      @sub "#bullet@conv/status/part", (e,args) =>
         @empty()

      @sub "#bullet@conv/status/join", (e, args) =>
         @rpc "#bullet@conv_history"
         @empty()

      @wait_ajax_done () =>
         @debug "AJAX DONE"
   
   displayName: (id,name,email) -> if name then name else if email then email else id
 
   append: (user, msg) ->
      [id,name,email] = user
      [stamp,text] = msg
      text_div = $("<div>").addClass(@a.text).html(text)
      stamp_div = $("<div>").addClass(@a.stamp).html(stamp + "&nbsp;&nbsp;" + @displayName(id,name,email) + ":")
      holder = $("<div>").addClass("row")
      holder.prepend text_div
      holder.prepend stamp_div
      @e.prepend holder


