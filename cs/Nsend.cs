define ["pi/Pi"], (aPi) -> class Nsend extends aPi
   
   nsend: (msg, callback) -> @rpc "#bullet@self", [], (bullet) => bullet.nsend msg, callback

   ncall: (callback) -> @rpc "#bullet@self", [], (bulletObj) -> callback bulletObj

   error: (m...) -> 
      @rt.append "dialog/error", text: m.join(" ")

   info: (m...) -> 
      @rt.append "dialog/info", text: m.join(" ")

