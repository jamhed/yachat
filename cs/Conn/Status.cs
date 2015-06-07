define ["Nsend"], (Pi) -> class ConnStatus extends Pi

   init: ->
      @sub "#bullet@conn/open", => @e.css color: "#fff"
      @sub "#bullet@conn/close", => @e.css color: "#aaa"

