#!/bin/sh

batt="$(cat /sys/class/power_supply/BAT0/capacity)"


if [ "$batt" -gt "100" ]; then
	icon=""
elif [ "$batt" -gt "90" ]; then
	icon=""
elif [ "$batt" -gt "80" ]; then
	icon=""
elif [ "$batt" -gt "70" ]; then
	icon=""
elif [ "$batt" -gt "60" ]; then
	icon=""
elif [ "$batt" -gt "50" ]; then
	icon=""
elif [ "$batt" -gt "40" ]; then
	icon=""
elif [ "$batt" -gt "30" ]; then
	icon=""
elif [ "$batt" -gt "20" ]; then
	icon=""
elif [ "$batt" -gt "10" ]; then
	icon=""
else 
	icon="" && echo " charge me" | dnote -loc 5
fi

if [ "$batt" -gt "100" ]; then
	col="#50fa7b"
elif [ "$batt" -gt "90" ]; then
	col="#50fa7b"
elif [ "$batt" -gt "80" ]; then
	col=""
elif [ "$batt" -gt "70" ]; then
	col=""
elif [ "$batt" -gt "60" ]; then
	col=""
elif [ "$batt" -gt "50" ]; then
	col=""
elif [ "$batt" -gt "40" ]; then
	col=""
elif [ "$batt" -gt "30" ]; then
	col=""
elif [ "$batt" -gt "20" ]; then
	col=""
elif [ "$batt" -gt "10" ]; then
	col=""
else 
	col="" && echo " charge me" | dnote -loc 5
fi

echo " ^b#44475a^^c#f8f8f2^ $icon ^b$col^^c#282A36^ $batt% " 
