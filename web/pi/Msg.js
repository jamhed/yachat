// Generated by CoffeeScript 1.9.3
var extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

define(["Nsend", "Cmon"], function(Pi, Cmon) {
  var Msg;
  return Msg = (function(superClass) {
    extend(Msg, superClass);

    function Msg() {
      return Msg.__super__.constructor.apply(this, arguments);
    }

    Msg.prototype.init = function() {
      Msg.__super__.init.apply(this, arguments);
      return this.e.keyup((function(_this) {
        return function(e) {
          if (e.keyCode === 13) {
            _this.send("msg/conv", Cmon.sid(), Cmon.conv_id(), _this.e.val());
            return _this.e.val("");
          }
        };
      })(this));
    };

    return Msg;

  })(Pi);
});
