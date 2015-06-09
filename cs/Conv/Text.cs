define ["Nsend", "Cmon"], (Pi, Cmon) -> class ConvText extends Pi

   attr: -> super.concat ["stamp", "text"]

   draw: (rows) ->
      @clear()
      for row in rows.reverse()
         [user, msg] = row
         @append user, msg

   query: -> @nsend ["conv/history", Cmon.sid(), Cmon.conv_id()], (status, rows) => @draw rows

   init: ->
      @sub "#bullet@new_msg", (e, args) =>
         [convId, user, msg] = args
         @append user, msg

      @sub "#bullet@sys_msg", (e, args) =>
         [convId,user,msg] = args 
         @append user, msg


      @sub "#bullet@conv/status/part", (e,args) => @clear()

      @sub "#bullet@conv/status/join", (e, args) => @query()

      @sub "#bullet@user/status/registered", (ev, args) => @query()

      @sub "#bullet@user/status/anonymous", (ev, args) => @query()

      @sub "#bullet@user/status/not_logged", (ev, args) => @clear()


   append: (user, msg) ->
      [stamp,text] = msg
      text_div = $("<div>").addClass(@a.text).html(text)
      stamp_div = $("<div>").addClass(@a.stamp).html(stamp + "&nbsp;&nbsp;" + Cmon.displayNameA(user) + ":")
      holder = $("<div>").addClass("row")
      holder.prepend text_div
      holder.prepend stamp_div
      @e.prepend holder


