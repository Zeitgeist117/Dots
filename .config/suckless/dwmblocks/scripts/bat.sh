#!/bin/sh

batt="$(cat /sys/class/power_supply/BAT1/capacity)"


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


echo " $icon $batt%" 
