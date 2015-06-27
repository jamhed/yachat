// Generated by CoffeeScript 1.9.3
var extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

define(["Nsend", "pi/m/Source"], function(aPi, mSource) {
  var ConvStatus;
  return ConvStatus = (function(superClass) {
    extend(ConvStatus, superClass);

    function ConvStatus() {
      return ConvStatus.__super__.constructor.apply(this, arguments);
    }

    ConvStatus.prototype.attr = function() {
      return ConvStatus.__super__.attr.apply(this, arguments).concat(["join", "leave", "display"]);
    };

    ConvStatus.prototype.init = function() {
      ConvStatus.__super__.init.apply(this, arguments);
      this.bsub("conv/status/join", (function(_this) {
        return function(ev, convId) {
          $(_this.a.display).val(convId);
          _this.e.html(mSource.get(_this.a.leave));
          return _this.rt.pi(_this.e);
        };
      })(this));
      this.bsub("conv/status/part", (function(_this) {
        return function(ev, args) {
          $(_this.a.display).val("");
          _this.e.html(mSource.get(_this.a.join));
          return _this.rt.pi(_this.e);
        };
      })(this));
      return this.bsub("user/status/not_logged", (function(_this) {
        return function(ev, args) {
          _this.e.html(mSource.get(_this.a.join));
          return _this.rt.pi(_this.e);
        };
      })(this));
    };

    return ConvStatus;

  })(aPi);
});
