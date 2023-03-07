#!/bin/sh
music="$(mpc current)"
icon=$(mpc | grep "\[playing\]" > /dev/null && echo "契" || echo "")
 
echo "^d^^b#44475a^^c#f8f8f2^ $icon ^b#8be9fd^^c#282A36^ $music" 
