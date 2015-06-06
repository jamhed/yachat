define ["Nsend", "pi/m/Source", "Cmon"], (aPi, mSource, Cmon) -> class ConvList extends aPi

   attr: -> super.concat ["view"]

   draw: (List) ->
      tmpl = @rt.source @a.view

      @clear()
      for convId in List
         @e.append tmpl {id: convId}
     
      @rt.pi @e

   query: -> @nsend ["user/conv_list", Cmon.user_id()], (status, List) => @draw List

   autojoin: (List) ->
      storedConvId = Cmon.conv_id()
      for convId in List
         if storedConvId == convId
            @rpc "#bullet@pub_event", ["conv/status/join", convId]
 
   init: ->

      @sub "#bullet@conv/status/join", (ev, args) => @query()

      @sub "#bullet@conv/status/part", (ev, args) => @query()
      
      @sub "#bullet@user/status/registered", (ev, args) => @query()

      @sub "#bullet@user/status/anonymous", (ev, args) => @query()

      @sub "#bullet@user/status/not_logged", (ev, args) => @clear()

      @wait_ajax_done =>
         @nsend ["user/conv_list", Cmon.user_id()], (status, List) => @autojoin List
