define ["pi/Pi", "pi/m/Source"], (aPi, mSource) -> class ConvList extends aPi

   attr: -> super.concat ["view"]

   init: ->
      @sub "#bullet@conv/users", (ev, args) =>
         @debug "USERS", args
         @empty()
         [status, List] = args
         tmpl = @rt.source @a.view
         @e.append tmpl {id: id, display: @displayName(id,name,email) } for [id,name,email] in List
         @rt.pi @e

      @sub "#bullet@conv/status/join", (ev, args) =>
         @rpc "#bullet@query_conv_users"

      @sub "#bullet@conv/status/part", (ev, args) =>
         @rpc "#bullet@query_conv_users"

   displayName: (id,name,email) -> if name != "undefined" then name else if email != "undefined" then email else id
