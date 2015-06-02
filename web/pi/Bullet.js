// Generated by CoffeeScript 1.9.3
var extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty,
  slice = [].slice,
  indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; };

define(["pi/Pi", "/js/bullet.js", "Util"], function(Pi, Bullet, Util) {
  return Bullet = (function(superClass) {
    extend(Bullet, superClass);

    function Bullet() {
      return Bullet.__super__.constructor.apply(this, arguments);
    }

    Bullet.prototype.attr = function() {
      return Bullet.__super__.attr.apply(this, arguments).concat(["uri"]);
    };

    Bullet.prototype.init = function() {
      this.uri = this.a.uri || "ws://" + window.location.hostname + ":" + window.location.port + "/main/ws/";
      this.bullet = $.bullet(this.uri, {
        disableWebSocket: false,
        disableEventSource: true,
        disableXHRPolling: true
      });
      this.bullet.onopen = (function(_this) {
        return function() {
          console.log("conn()");
          _this.user_status("not_logged");
          return _this.check_user_id();
        };
      })(this);
      this.bullet.ondisconnect = (function(_this) {
        return function() {
          return _this.user_status("not_logged");
        };
      })(this);
      this.bullet.onclose = (function(_this) {
        return function() {
          return _this.user_status("not_logged");
        };
      })(this);
      this.bullet.onmessage = (function(_this) {
        return function(e) {
          var args, data, msg;
          if (e.data !== "ping") {
            data = JSON.parse(e.data);
            msg = data[0], args = 2 <= data.length ? slice.call(data, 1) : [];
            return _this.event(msg, args);
          }
        };
      })(this);
      this.bullet.onheartbeat = (function(_this) {
        return function() {
          return _this.bullet.send("ping");
        };
      })(this);
      this.sub("#bullet@user/new", (function(_this) {
        return function(e, args) {
          var status, userId;
          status = args[0], userId = args[1];
          if (status === "new") {
            _this.set_user_id(userId);
            return _this.user_status("anonymous");
          } else {
            _this.user_status = "not_logged";
            return _this.error("Server protocol");
          }
        };
      })(this));
      this.sub("#bullet@user", (function(_this) {
        return function(e, args) {
          var status, userId;
          status = args[0], userId = args[1];
          if (status === "fail") {
            return _this.send("user/new");
          } else {
            return _this.send("user/info", userId);
          }
        };
      })(this));
      this.sub("#bullet@user/info", (function(_this) {
        return function(e, args) {
          var email, name, ref, status, userId;
          status = args[0], (ref = args[1], userId = ref[0], name = ref[1], email = ref[2]);
          _this.set_user_id(userId);
          if (email === "undefined") {
            return _this.user_status("anonymous", [userId]);
          } else {
            return _this.user_status("registered", [userId, name, email]);
          }
        };
      })(this));
      this.sub("#bullet@user/login", (function(_this) {
        return function(e, args) {
          var status, userId;
          status = args[0], userId = args[1];
          if (status === "ok") {
            return _this.send("user/info", userId);
          } else {
            return _this.error("Login or password error: " + cause);
          }
        };
      })(this));
      this.sub("#bullet@user/conv_list", (function(_this) {
        return function(e, args) {
          var convId, convList, status;
          status = args[0], convList = args[1];
          convId = parseInt(_this.globalGet("conv_id"));
          if (indexOf.call(convList, convId) >= 0) {
            return _this.conv_status("join", convId);
          } else {
            return _this.conv_status("part");
          }
        };
      })(this));
      this.sub("#bullet@conv/new", (function(_this) {
        return function(e, args) {
          var convId, status;
          status = args[0], convId = args[1];
          _this.set_conv_id(convId);
          return _this.conv_status("join", convId);
        };
      })(this));
      this.sub("#bullet@conv/join", (function(_this) {
        return function(e, args) {
          var convId, status;
          status = args[0], convId = args[1];
          _this.set_conv_id(convId);
          return _this.conv_status("join", convId);
        };
      })(this));
      return this.sub("#bullet@conv/leave", (function(_this) {
        return function(e, args) {
          var convId, status;
          status = args[0], convId = args[1];
          _this.set_conv_id(null);
          return _this.conv_status("part", convId);
        };
      })(this));
    };

    Bullet.prototype.check_user_id = function() {
      var userId;
      userId = parseInt(this.globalGet("user_id"));
      if (userId) {
        return this.send("user", userId);
      }
    };

    Bullet.prototype.user_status = function(status, userRec) {
      console.log("user status:", status);
      this._user_status = status;
      return this.event("user/status/" + status, userRec);
    };

    Bullet.prototype.conv_status = function(status, convId) {
      this._conv_status = status;
      console.log("conv/status/" + status, convId);
      return this.event("conv/status/" + status, convId);
    };

    Bullet.prototype.set_conv_id = function(convId) {
      this.globalSet("conv_id", convId);
      return this.conv_id = convId;
    };

    Bullet.prototype.set_user_id = function(userId) {
      this.globalSet("user_id", userId);
      return this.user_id = userId;
    };

    Bullet.prototype.error = function() {
      var m;
      m = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      return this.rt.append("dialog/error", {
        text: m
      });
    };

    Bullet.prototype.send = function() {
      var msg;
      msg = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      return this.bullet.send(JSON.stringify(msg));
    };

    Bullet.prototype.query_convs = function() {
      var userId;
      userId = parseInt(this.globalGet("user_id"));
      return this.send("user/conv_list", userId);
    };

    Bullet.prototype.join_conv = function() {
      var chatId;
      chatId = parseInt($("#chatId").val());
      if (chatId) {
        return this.send("conv/join", this.user_id, chatId);
      } else {
        return this.send("conv/new", this.user_id);
      }
    };

    Bullet.prototype.leave_conv = function() {
      return this.send("conv/leave", this.user_id, this.convId);
    };

    Bullet.prototype.login = function() {
      var a, h;
      a = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      h = (new Util()).list2hash(a);
      return this.send("user/login", h.email, h.password);
    };

    Bullet.prototype.logout = function() {
      this.send("user/logout");
      this.set_user_id(null);
      return this.user_status("not_logged");
    };

    Bullet.prototype.register_email = function() {
      var a, h;
      a = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      h = (new Util()).list2hash(a);
      return this.send("user/register", this.user_id, h.email, h.password, h.username, h.gender);
    };

    Bullet.prototype.send_msg = function(msg) {
      return this.send("msg/conv", this.user_id, this.convId, msg);
    };

    return Bullet;

  })(Pi);
});
