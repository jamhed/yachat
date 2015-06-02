define ["pi/Pi"], (Pi) -> class Register extends Pi

   attr: -> super.concat ["row"]

   init: ->
      @sub "#bullet@user/register", (e, args) =>
         [status] = args

   error: (m...) -> 
      @rt.append "dialog/error", text: m
