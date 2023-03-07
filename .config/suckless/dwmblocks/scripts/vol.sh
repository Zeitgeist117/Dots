#!/bin/sh

vol="$(pamixer --get-volume)"
function send_notification() {
	volume=$(pamixer --get-volume)
}

if [ "$vol" -gt "70" ]; then
	icon="墳"
elif [ "$vol" -gt "30" ]; then
	icon="奔"
elif [ "$vol" -gt "0" ]; then
	icon="奄"
else
    icon="" 
fi

[ $(pamixer --get-mute) = true ] && echo "          " | dnote -loc 5 -pbar $vol 100 && exit

# echo "$icon $vol%"
echo "   $icon $vol%   " | dnote -loc 5 -pbar $vol 100
