// Generated by CoffeeScript 1.9.3
var extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  hasProp = {}.hasOwnProperty;

define(["pi/Pi"], function(aPi) {
  var aForm;
  return aForm = (function(superClass) {
    extend(aForm, superClass);

    aForm.prototype.attr = function() {
      return aForm.__super__.attr.apply(this, arguments).concat(["el", "target"]);
    };

    function aForm() {
      aForm.__super__.constructor.apply(this, arguments);
      this.sub("init", (function(_this) {
        return function(ev, data) {
          var k, v;
          return _this.deserialize($(_this.a.el), (function() {
            var results;
            results = [];
            for (k in data) {
              v = data[k];
              results.push({
                name: k,
                value: v
              });
            }
            return results;
          })());
        };
      })(this));
      this.sub("reset", (function(_this) {
        return function(ev, data) {
          _this.reset($(_this.a.el));
          if (_this.a.target) {
            return _this.pub(_this.a.target, {
              data: {}
            });
          }
        };
      })(this));
    }

    aForm.prototype.serialize = function(inputs) {
      var _i, fn, j, len, ret;
      ret = {};
      fn = function(i) {
        var group, item_val, tmp;
        group = i.data("group") || "form";
        if (!ret[group]) {
          ret[group] = [];
        }
        if (i.attr("type") === "checkbox") {
          if (i.prop("checked")) {
            return ret[group].push({
              name: i.attr("name"),
              value: true,
              "class": i.attr("class")
            });
          } else {
            return ret[group].push({
              name: i.attr("name"),
              value: false,
              "class": i.attr("class")
            });
          }
        } else if (i.attr("type") === "radio") {
          if (i.prop("checked")) {
            return ret[group].push({
              name: i.attr("name"),
              value: i.attr("value"),
              "class": i.attr("class")
            });
          }
        } else {
          tmp = {
            name: i.attr("id"),
            value: i.val()
          };
          if (i.attr("item_map")) {
            item_val = i.attr("item_val");
            ret[group].push({
              name: i.attr("item_map"),
              "class": i.attr("class"),
              value: item_val === "null" ? null : item_val
            });
          }
          return ret[group].push(tmp);
        }
      };
      for (j = 0, len = inputs.length; j < len; j++) {
        _i = inputs[j];
        fn($(_i));
      }
      return ret;
    };

    aForm.prototype.deserialize = function(inputs, data) {
      var _i, d, hdata, j, l, len, len1, results;
      hdata = {};
      for (j = 0, len = data.length; j < len; j++) {
        d = data[j];
        hdata[d.name] = d;
      }
      results = [];
      for (l = 0, len1 = inputs.length; l < len1; l++) {
        _i = inputs[l];
        results.push((function(i) {
          var f;
          if (i.attr("type") === "checkbox" && hdata[i.attr("name")]) {
            return i.attr("checked", 1);
          } else {
            if (f = hdata[i.attr("name")]) {
              i.val(f.value);
              if (f["item_val"]) {
                return i.attr("item_val", f["item_val"]);
              }
            }
          }
        })($(_i)));
      }
      return results;
    };

    aForm.prototype.reset = function(inputs) {
      var _i, j, len, results;
      results = [];
      for (j = 0, len = inputs.length; j < len; j++) {
        _i = inputs[j];
        results.push((function(i) {
          if (i.attr("type") === "checkbox") {
            return i.removeAttr("checked");
          } else {
            if (i.attr("item_map")) {
              i.attr("item_val", "");
            }
            return i.val("");
          }
        })($(_i)));
      }
      return results;
    };

    aForm.prototype.init = function() {
      return this.e.click((function(_this) {
        return function(ev) {
          return _this.onClick(ev);
        };
      })(this));
    };

    aForm.prototype.onClick = function(ev) {
      var data;
      ev.preventDefault();
      data = this.serialize($(this.a.el));
      this.rpc(this.a.target, data.form, function() {});
      return data;
    };

    return aForm;

  })(aPi);
});
