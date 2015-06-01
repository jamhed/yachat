// Generated by CoffeeScript 1.9.3
var extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

define(["pi/Pi", "pi/lib/jquery-ui"], function(aPi, jqUI) {
  var DialogSimple;
  return DialogSimple = (function(superClass) {
    extend(DialogSimple, superClass);

    function DialogSimple() {
      return DialogSimple.__super__.constructor.apply(this, arguments);
    }

    DialogSimple.prototype.init = function() {
      return this.e.dialog({
        close: (function(_this) {
          return function(ev, ui) {
            return _this.e.remove();
          };
        })(this),
        draggable: true,
        width: this.data.width || 500,
        height: this.data.height || "auto",
        position: {
          my: "top",
          at: "top+150"
        }
      });
    };

    DialogSimple.prototype.close = function() {
      return this.e.remove();
    };

    return DialogSimple;

  })(aPi);
});
