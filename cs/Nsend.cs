define ["pi/Pi"], (aPi) -> class Nsend extends aPi

	attr: -> super.concat ['bullet']

	init: ->
		if ! @a.bullet
			@a.bullet = "#bullet"

	nsend: (msg, callback) -> @rpc "#{@a.bullet}@self", [], (b) => b.nsend msg, callback

	ncall: (callback) -> @rpc "#{@a.bullet}@self", [], (b) -> callback(b)

	send: (msg...) -> @rpc "#{@a.bullet}@self", [], (b) => b.send msg...

	bsub: (ev, callback) -> @sub "#{@a.bullet}@#{ev}", callback

	brpc: (method, args, callback) -> @rpc "#{@a.bullet}@#{method}", args, callback

	error: (m...) -> 
		@append "dialog/error", text: m.join(" ")

	info: (m...) -> 
		@append "dialog/info", text: m.join(" ")
