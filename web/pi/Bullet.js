// Generated by CoffeeScript 1.9.3
var bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
  extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty,
  slice = [].slice;

define(["pi/Pi", "/js/bullet.js", "Cmon"], function(Pi, Bullet, Cmon) {
  return Bullet = (function(superClass) {
    extend(Bullet, superClass);

    function Bullet() {
      this.event = bind(this.event, this);
      return Bullet.__super__.constructor.apply(this, arguments);
    }

    Bullet.prototype.seq = 0;

    Bullet.prototype.cb_nsend = null;

    Bullet.prototype.attr = function() {
      return Bullet.__super__.attr.apply(this, arguments).concat(["uri"]);
    };

    Bullet.prototype.init = function() {
      this.cb_nsend = {};
      this.uri = this.a.uri || "ws://" + window.location.hostname + ":" + window.location.port + "/main/ws/";
      this.bullet = $.bullet(this.uri, {
        disableWebSocket: false,
        disableEventSource: true,
        disableXHRPolling: true
      });
      this.bullet.onopen = (function(_this) {
        return function() {
          _this.debug("conn()");
          return _this.wait_ajax_done(function() {
            _this.user_status("not_logged");
            return _this.check_user_id();
          });
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
      this.handler("nmsg", (function(_this) {
        return function(e, data) {
          var args, msg, ref, ref1, seq;
          seq = data[0], (ref = data[1], msg = ref[0], args = 2 <= ref.length ? slice.call(ref, 1) : []);
          _this.debug("NMSG:", seq, msg, args);
          if (_this.cb_nsend[seq]) {
            if (_this.cb_nsend[seq].msg === msg) {
              (ref1 = _this.cb_nsend[seq]).fn.apply(ref1, args);
            } else {
              _this.debug("NMSG:", "unmatched message for seq", seq, msg, _this.cb_nsend[seq].msg, args);
            }
            return delete _this.cb_nsend[seq];
          } else {
            return _this.error("no callback for seq", seq);
          }
        };
      })(this));
      this.handler("sys_msg", (function(_this) {
        return function(e, args) {
          var cid, ev, ref, stamp, user;
          return cid = args[0], user = args[1], (ref = args[2], stamp = ref[0], ev = ref[1]), args;
        };
      })(this));
      this.handler("user/new", (function(_this) {
        return function(e, args) {
          var status, userId;
          status = args[0], userId = args[1];
          if (status === "new") {
            Cmon.set_user_id(userId);
            return _this.user_status("anonymous", [userId]);
          } else {
            _this.user_status = "not_logged";
            return _this.error("Server protocol");
          }
        };
      })(this));
      this.handler("user", (function(_this) {
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
      this.handler("user/info", (function(_this) {
        return function(e, args) {
          var email, name, ref, status, userId;
          status = args[0], (ref = args[1], userId = ref[0], name = ref[1], email = ref[2]);
          Cmon.set_user_id(userId);
          if (email) {
            return _this.user_status("registered", [userId, name, email]);
          } else {
            return _this.user_status("anonymous", [userId]);
          }
        };
      })(this));
      this.handler("user/register", (function(_this) {
        return function(e, args) {
          var status, userId;
          status = args[0], userId = args[1];
          if (status === "ok") {
            _this.send("user/info", userId);
            return window.location = "#";
          } else {
            return _this.error("Register Error", userId);
          }
        };
      })(this));
      this.handler("user/login", (function(_this) {
        return function(e, args) {
          var status, userId;
          status = args[0], userId = args[1];
          if (status === "ok") {
            return _this.send("user/info", userId);
          } else {
            return _this.error("Login or password error: " + userId);
          }
        };
      })(this));
      this.handler("conv/new", (function(_this) {
        return function(e, args) {
          var convId, status;
          status = args[0], convId = args[1];
          if (status === "ok") {
            Cmon.set_conv_id(convId);
            return _this.conv_status("join", convId);
          } else {
            Cmon.set_conv_id(null);
            return _this.conv_status("part");
          }
        };
      })(this));
      this.handler("conv/join", (function(_this) {
        return function(e, args) {
          var convId, status;
          status = args[0], convId = args[1];
          Cmon.set_conv_id(convId);
          return _this.conv_status("join", convId);
        };
      })(this));
      this.handler("conv/leave", (function(_this) {
        return function(e, args) {
          var convId, status;
          status = args[0], convId = args[1];
          _this.conv_status("part", convId);
          return Cmon.set_conv_id(null);
        };
      })(this));
      return this.handler("user/logout", (function(_this) {
        return function(e, args) {
          _this.conv_status("part", null);
          return Cmon.set_conv_id(null);
        };
      })(this));
    };

    Bullet.prototype.check_user_id = function() {
      var userId;
      userId = Cmon.user_id();
      if (userId) {
        return this.send("user", userId);
      }
    };

    Bullet.prototype.user_status = function(status, userRec) {
      this.debug("user status:", status);
      this._user_status = status;
      return this.event("user/status/" + status, userRec);
    };

    Bullet.prototype.conv_status = function(status, convId) {
      this._conv_status = status;
      this.debug("conv status:", status, convId);
      return this.event("conv/status/" + status, convId);
    };

    Bullet.prototype.error = function() {
      var m;
      m = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      return this.rt.append("dialog/error", {
        text: m.join(" ")
      });
    };

    Bullet.prototype.event = function(e, args) {
      this.debug("EVENT", e, args);
      return Bullet.__super__.event.call(this, e, args);
    };

    Bullet.prototype.send = function() {
      var msg;
      msg = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      this.debug("MSG", msg);
      return this.bullet.send(JSON.stringify(msg));
    };

    Bullet.prototype.nsend = function(msg, callback) {
      this.seq = this.seq + 1;
      this.debug("N-MSG", this.seq, msg);
      this.cb_nsend[this.seq] = {
        fn: callback,
        msg: msg[0]
      };
      return this.bullet.send(JSON.stringify(["nmsg", this.seq, msg]));
    };

    Bullet.prototype.join_conv = function(conv) {
      var chatId;
      chatId = conv.conv ? conv.conv : parseInt($("#chatId").val());
      if (chatId) {
        return this.send("conv/join", Cmon.user_id(), chatId);
      } else {
        if (Cmon.user_id()) {
          return this.send("conv/new", Cmon.user_id());
        }
      }
    };

    Bullet.prototype.leave_conv = function() {
      return this.send("conv/leave", Cmon.user_id(), Cmon.conv_id());
    };

    Bullet.prototype.pub_event = function() {
      var args, ev;
      ev = arguments[0], args = 2 <= arguments.length ? slice.call(arguments, 1) : [];
      return this.event(ev, args);
    };

    Bullet.prototype.anonymous = function() {
      return this.send("user/new");
    };

    Bullet.prototype.login = function() {
      var a, h;
      a = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      h = Cmon.list2hash(a);
      return this.send("user/login", h.email, h.password);
    };

    Bullet.prototype.logout = function() {
      this.send("user/logout");
      Cmon.set_user_id(null);
      this.user_status("not_logged");
      return window.location = "#";
    };

    Bullet.prototype.register_email = function() {
      var a, h;
      a = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      h = Cmon.list2hash(a);
      return this.send("user/register", Cmon.user_id(), h.email, h.password, h.firstname, h.lastname, h.username, h.gender);
    };

    Bullet.prototype.send_msg = function(msg) {
      return this.send("msg/conv", Cmon.user_id(), Cmon.conv_id(), msg);
    };

    return Bullet;

  })(Pi);
});
