// Generated by CoffeeScript 1.9.3
var extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty,
  slice = [].slice;

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
        disableWebSocket: false
      });
      this.bullet.onopen = (function(_this) {
        return function() {
          console.log("conn()");
          return _this.get_user_id();
        };
      })(this);
      this.bullet.ondisconnect = (function(_this) {
        return function() {};
      })(this);
      this.bullet.onclose = (function(_this) {
        return function() {};
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
          var number, status;
          status = args[0], number = args[1];
          if (status === "new") {
            _this.user_id = number;
            return _this.globalSet("user_id", _this.user_id);
          } else {
            return _this.error("Cannot Get UID");
          }
        };
      })(this));
      this.sub("#bullet@user", (function(_this) {
        return function(e, args) {
          var status;
          status = args[0];
          if (status === "fail") {
            _this.error("Cannot confirm UID, request New");
            return _this.send("user/new");
          } else {
            _this.send("user/info", _this.user_id);
            return _this.event("login", _this.user_id);
          }
        };
      })(this));
      this.sub("#bullet@conv/new", (function(_this) {
        return function(e, args) {
          var convId, status;
          status = args[0], convId = args[1];
          return _this.convId = convId;
        };
      })(this));
      this.sub("#bullet@conv/join", (function(_this) {
        return function(e, args) {
          var convId, status;
          status = args[0], convId = args[1];
          return _this.convId = convId;
        };
      })(this));
      this.sub("#bullet@user/info", (function(_this) {
        return function(e, args) {
          var email, name, ref, status, userId;
          status = args[0], (ref = args[1], userId = ref[0], name = ref[1], email = ref[2]);
          if (!email) {
            return _this.event("anonymous", _this.user_id);
          }
        };
      })(this));
      this.sub("#bullet@user/login", (function(_this) {
        return function(e, args) {
          var status, user_id;
          status = args[0], user_id = args[1];
          if (status === "ok") {
            _this.user_id = user_id;
            _this.send("user/info", _this.user_id);
            return _this.event("login", _this.user_id);
          } else {
            return _this.error("Login or password error");
          }
        };
      })(this));
      return console.log(this.bullet.transport());
    };

    Bullet.prototype.get_user_id = function() {
      this.user_id = parseInt(this.globalGet("user_id"));
      if (!this.user_id) {
        return this.send("user/new");
      } else {
        return this.send("user", this.user_id);
      }
    };

    Bullet.prototype.send = function() {
      var msg;
      msg = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      return this.bullet.send(JSON.stringify(msg));
    };

    Bullet.prototype.new_conv = function() {
      var chatId;
      chatId = parseInt($("#chatId").val());
      if (chatId) {
        return this.send("conv/join", this.user_id, chatId);
      } else {
        return this.send("conv/new", this.user_id);
      }
    };

    Bullet.prototype.login = function() {
      var a, h;
      a = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      h = (new Util()).list2hash(a);
      return this.send("user/login", h.email, h.password);
    };

    Bullet.prototype.register_email = function() {
      var a, h;
      a = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      h = (new Util()).list2hash(a);
      return this.send("user/register", this.user_id, h.email, h.password, h.username, h.gender);
    };

    Bullet.prototype.send_msg = function(msg) {
      console.log("send_msg", msg);
      return this.send("msg/conv", this.user_id, this.convId, msg);
    };

    Bullet.prototype.error = function() {
      var m;
      m = 1 <= arguments.length ? slice.call(arguments, 0) : [];
      return this.rt.append("dialog/error", {
        text: m
      });
    };

    return Bullet;

  })(Pi);
});
