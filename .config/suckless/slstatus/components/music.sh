#!/bin/sh
music="$(mpc current)"
icon=$(mpc | grep "\[playing\]" > /dev/null && echo "契" || echo "")

echo $icon $music
