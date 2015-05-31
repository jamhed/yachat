define({
   catchError: true,
   paths: {
      "pi":                "pi",
      "pi/lib/jquery":     "lib/jquery/dist/jquery.min",
      "pi/lib/jquery-ui":  "lib/jquery-ui/jquery-ui.min",
      "pi/lib/doT":        "lib/doT/doT.min",
      "pi/lib/bootstrap":  "lib/bootstrap2.3.2/bootstrap/js/bootstrap.min",
      "pi/lib/URI":        "lib/uri.js/src",
      "pi/lib/markdown":   "lib/markdown/lib/markdown",
      "pi/lib/textile":    "lib/textile-js/lib/textile",
      "pi/lib/upload":     "lib/jquery-file-upload/js/jquery.fileupload",
      "jquery.ui.widget": "lib/jquery-file-upload/js/vendor/jquery.ui.widget"
   },
   shim: {
      "pi/lib/jquery": {
         exports: "jQuery"
      },
      "pi/lib/jquery-ui": {
         deps: ["pi/lib/jquery"]
      },
      "pi/lib/bootstrap": {
         deps: ["pi/lib/jquery"]
      },
      "pi/lib/markdown": {
         exports: "markdown"
      },
      "pi/lib/textile": {
         exports: "textile"
      },
   }
});
