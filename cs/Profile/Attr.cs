define ["pi/Pi"], (Pi) -> class ProfileAttr extends Pi
   
	attr: -> super.concat ["target"]

	init: ->
   		super
    	@e.click (ev, args) => @rpc @a.target
