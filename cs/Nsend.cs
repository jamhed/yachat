define ["pi/Pi"], (aPi) -> class Nsend extends aPi
   
   nsend: (msg, callback) -> @rpc "#bullet@self", [], (bullet) => bullet.nsend msg, callback
