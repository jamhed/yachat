// Generated by CoffeeScript 1.9.3
var extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

define(["pi/Pi"], function(aPi) {
  var Login;
  return Login = (function(superClass) {
    extend(Login, superClass);

    function Login() {
      return Login.__super__.constructor.apply(this, arguments);
    }

    Login.prototype.attr = function() {
      return Login.__super__.attr.apply(this, arguments).concat(["target", "dialog"]);
    };

    Login.prototype.init = function() {
      return this.e.click((function(_this) {
        return function(ev) {
          return _this.rpc("#bullet@self", [], function(bullet) {
            var h;
            h = bullet.fb_status === "connected" ? {
              fb_connect: 1
            } : {
              fb_connect: 0
            };
            _this.rt.append(_this.a.dialog, h);
            return _this.rt.pi(_this.e);
          });
        };
      })(this));
    };

    return Login;

  })(aPi);
});
