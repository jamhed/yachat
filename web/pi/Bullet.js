// Generated by CoffeeScript 1.9.3
var bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
  extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty,
  slice = [].slice;

define(["Nsend", "/js/bullet.js", "Cmon", "//connect.facebook.net/en_US/sdk.js"], function(Pi, Bullet, Cmon) {
  return Bullet = (function(superClass) {
    extend(Bullet, superClass);

    function Bullet() {
      this.event = bind(this.event, this);
      return Bullet.__super__.constructor.apply(this, arguments);
    }

    Bullet.prototype.seq = 0;

    Bullet.prototype.cb_nsend = null;

    Bullet.prototype.fb_status = null;

    Bullet.prototype.fb_token = null;

    Bullet.prototype.fb_fbid = null;

    Bullet.prototype._user_status = null;

    Bullet.prototype.attr = function() {
      return Bullet.__super__.attr.apply(this, arguments).concat(["uri", "fb_app"]);
    };

    Bullet.prototype.init = function() {
      FB.init({
        appId: this.a.fb_app,
        xfbml: true,
        version: "v2.3"
      });
      FB.getLoginStatus((function(_this) {
        return function(r) {
          return _this.handle_fb_auth(r);
        };
      })(this));
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
          return _this.event("conn/open");
        };
      })(this);
      this.bullet.ondisconnect = (function(_this) {
        return function() {
          _this.user_status("not_logged");
          return _this.event("conn/close");
        };
      })(this);
      this.bullet.onclose = (function(_this) {
        return function() {
          _this.user_status("not_logged");
          return _this.event("conn/close");
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
          var sessionId, status, userId;
          status = args[0], sessionId = args[1], userId = args[2];
          if (status === "ok") {
            Cmon.set_sid(sessionId);
            return _this.user_status("anonymous", [userId]);
          } else {
            _this.user_status = "not_logged";
            return _this.error("Server protocol");
          }
        };
      })(this));
      this.handler("user/get", (function(_this) {
        return function(e, args) {
          var sessionId, status, userInfo;
          status = args[0], sessionId = args[1], userInfo = args[2];
          if (status === "fail") {
            Cmon.set_sid(null);
            return _this.user_status("not_logged");
          }
          Cmon.set_sid(sessionId);
          if (userInfo[1]) {
            return _this.user_status("registered", userInfo);
          } else {
            return _this.user_status("anonymous", userInfo);
          }
        };
      })(this));
      this.handler("user/fb", (function(_this) {
        return function(e, args) {
          var sessionId, status, userInfo;
          status = args[0], sessionId = args[1], userInfo = args[2];
          if (status === "ok") {
            Cmon.set_sid(sessionId);
            return _this.user_status("registered", userInfo);
          } else {
            Cmon.set_sid(null);
            _this.error("Account is not found, please re-register");
            return _this.user_status("not_logged");
          }
        };
      })(this));
      this.handler("user/login", (function(_this) {
        return function(e, args) {
          var sessionId, status, userInfo;
          status = args[0], sessionId = args[1], userInfo = args[2];
          if (status === "ok") {
            Cmon.set_sid(sessionId);
            return _this.user_status("registered", userInfo);
          } else {
            Cmon.set_sid(null);
            _this.error("Login or password error: " + userId);
            return _this.user_status("not_logged");
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
          if (status === "ok") {
            Cmon.set_conv_id(convId);
            return _this.conv_status("join", convId);
          } else {
            _this.conv_status("part");
            return _this.error("Error join conversation!");
          }
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
          Cmon.set_conv_id(null);
          return window.location = "#";
        };
      })(this));
    };

    Bullet.prototype.check_user_id = function() {
      var sessionId;
      sessionId = Cmon.sid();
      if (sessionId) {
        return this.send("user/get", sessionId);
      } else {
        return this.user_status("not_logged");
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
        return this.send("conv/join", Cmon.sid(), chatId);
      } else {
        if (Cmon.sid()) {
          return this.send("conv/new", Cmon.sid());
        }
      }
    };

    Bullet.prototype.leave_conv = function() {
      return this.send("conv/leave", Cmon.sid(), Cmon.conv_id());
    };

    Bullet.prototype.pub_event = function() {
      var args, ev;
      ev = arguments[0], args = 2 <= arguments.length ? slice.call(arguments, 1) : [];
      return this.event(ev, args);
    };

    Bullet.prototype.anonymous = function() {
      return this.send("user/new", []);
    };

    Bullet.prototype.login = function() {
      var a, h;
      a = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      h = Cmon.list2hash(a);
      return this.send("user/login", [h.email, h.password]);
    };

    Bullet.prototype.logout = function() {
      this.send("user/logout", []);
      Cmon.set_sid(null);
      this.user_status("not_logged");
      return window.location = "#";
    };

    Bullet.prototype.send_msg = function(msg) {
      return this.send("msg/conv", Cmon.sid(), Cmon.conv_id(), msg);
    };

    Bullet.prototype.invite = function() {
      var a, h;
      a = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      h = Cmon.list2hash(a);
      return this.nsend(["user/email", h.email], (function(_this) {
        return function(status, user) {
          var email, id, name;
          if (status === "ok") {
            id = user[0], name = user[1], email = user[2];
            return _this.nsend(["conv/join", id, Cmon.conv_id()], function(status, convId) {
              return _this.event("conv/status/invite");
            });
          } else {
            return _this.error("Can't find user by email.");
          }
        };
      })(this));
    };

    Bullet.prototype.dialog_invite = function() {
      return this.rt.append("dialog/invite");
    };

    Bullet.prototype.register_facebook = function() {
      return FB.login(((function(_this) {
        return function(r) {
          return _this.handle_fb_register(r);
        };
      })(this)), {
        scope: "public_profile,email"
      });
    };

    Bullet.prototype.fb_login = function() {
      if (this.fb_status === "connected") {
        return this.send("user/fb", [this.fb_id]);
      } else {
        return this.error("Connect profile to facebook first!");
      }
    };

    Bullet.prototype.handle_fb_auth = function(r) {
      if (r.status === "connected") {
        this.fb_status = "connected";
        this.fb_token = r.authResponse.accessToken;
        return this.fb_id = r.authResponse.userID;
      }
    };

    Bullet.prototype.handle_fb_register = function(r) {
      if (r.status === "connected") {
        this.handle_fb_auth(r);
        return FB.api("/me", (function(_this) {
          return function(r) {
            return _this.nsend(["user/update", Cmon.sid(), "facebook_id", r.id, "email", r.email, "firstname", r.first_name, "lastname", r.last_name, "username", r.name, "gender", r.gender], function(r) {
              return _this.handle_fb_register_ok(r);
            });
          };
        })(this));
      } else {
        return this.error("Facebook status " + r.status);
      }
    };

    Bullet.prototype.handle_fb_register_ok = function(status) {
      if (status === "ok") {
        return this.send("user/profile", Cmon.sid());
      } else {
        return this.error("Error updating profile.");
      }
    };

    return Bullet;

  })(Pi);
});
