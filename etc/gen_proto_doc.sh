#!/bin/sh
grep -E "^msg|^%msg|^\s+\[M" src/ws_* | grep -v skip | grep -v Stub 
