#!/bin/bash

STATUS=$(playerctl status)
PLAY="playerctl play"
PAUSE="playerctl pause"

if [ "$STATUS" == "Playing" ]; then
    eval "$PAUSE"
    hyprctl notify -1 1500 "rgba(e78a4eFF)" "Media: Paused!"
elif [ "$STATUS" == "Paused" ]; then
    eval "$PLAY"
    hyprctl notify -1 1200 "rgba(e78a4eFF)" "Media: Resumed!"
else
   notify-send "We are so fucked!"
fi
