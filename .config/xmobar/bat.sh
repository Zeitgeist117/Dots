#!/bin/sh
# Prints all batteries, their percentage remaining and an emoji corresponding
# to charge status (🔌 for plugged up, 🔋 for discharging on battery, etc.).

notify() {
    notify-send -i battery-good-symbolic \
        -h string:x-canonical-private-synchronous:battery \
        "Battery" "$1" -t 4000
}

# Loop through all attached batteries and format the info
for battery in /sys/class/power_supply/BAT?*; do
    # If non-first battery, print a space separator.
    [ -n "${capacity+x}" ] && printf " "

    capacity="$(cat "$battery/capacity" 2>&1)"
    if [ "$capacity" -gt 90 ]; then
        status=" "
	color="50fa7b"
    elif [ "$capacity" -gt 60 ]; then
        status=" "
	color="50fa7b"
    elif [ "$capacity" -gt 40 ]; then
        status=" "
	color="50fa7b"
    elif [ "$capacity" -gt 10 ]; then
        status=" "
	color="50fa7b"
    else
        status=" "
	color="50fa7b"
    fi

    # Sets up the status and capacity
    case "$(cat "$battery/status" 2>&1)" in
        Full) status=" " ;;
        Discharging)
            if [ "$capacity" -le 20 ]; then
                status="$status"
            fi
            ;;
        Charging) status="󰚥 " ;;
        "Not charging") status=" " ;;
        Unknown) status="? $status" ;;
        *) exit 1 ;;
    esac

    # Prints the info
    # echo "$status$capacity%"
    echo "$status $capacity%" 
done && echo
