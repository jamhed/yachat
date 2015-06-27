define ["Nsend", "Cmon"], (Pi, Cmon) -> class ConvText extends Pi

   attr: -> super.concat ["stamp", "text"]

   draw: (rows) ->
      @clear()
      for row in rows.reverse()
         [user, msg] = row
         @append user, msg

   query: -> @nsend ["conv/history", Cmon.sid(), Cmon.conv_id()], (status, rows) => @draw rows

   init: ->
      super
      @bsub "new_msg", (e, args) =>
         [convId, user, msg] = args
         if convId == Cmon.conv_id()
            @append user, msg

      @bsub "conv_msg", (e, args) =>
         [convId, [stamp, [msg, user]]] = args
         if convId == Cmon.conv_id()
            @append user, [stamp, msg]

      @bsub "conv/status/part", (e,args) => @clear()
      @bsub "conv/status/join", (e, args) => @query()
      @bsub "user/status/registered", (ev, args) => @query()
      @bsub "user/status/anonymous", (ev, args) => @query()
      @bsub "user/status/not_logged", (ev, args) => @clear()

   append: (user, msg) ->
      [stamp,text] = msg
      text_div = $("<div>").addClass(@a.text).html(text)
      stamp_div = $("<div>").addClass(@a.stamp).html(stamp + "&nbsp;&nbsp;" + Cmon.displayNameA(user) + ":")
      holder = $("<div>").addClass("row")
      holder.prepend text_div
      holder.prepend stamp_div
      @e.prepend holder