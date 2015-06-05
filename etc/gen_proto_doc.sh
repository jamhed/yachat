#!/bin/sh
grep -E "^msg|^%msg|^\s+\[M" src/ws_* | sed s/^.*\:// | grep -v skip | grep -v Stub | ./filter_doc.pl
