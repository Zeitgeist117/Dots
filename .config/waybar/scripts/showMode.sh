#!/bin/sh

MODES="$(< $HOME/.config/hypr/mode.txt)"
# echo $MODES

KILLWAYBAR() {
  if [[ $MODES == "Normal Mode" ]]; then
    echo $MODES
  else  [[ $MODES == "Insert Mode" ]]
    echo $MODES
  fi
}
KILLWAYBAR

# if [[ $MODES == "Normal Mode" ]]; then
#   cp -r ~/.config/waybar/MODES/colors-Normal-Mode.css ~/.config/waybar/colors.css
#   pkill -SIGUSR2 waybar && exit
#   echo $MODES
# else  [[ $MODES == "Insert Mode" ]]
#   cp -r ~/.config/waybar/MODES/colors-Insert-Mode.css ~/.config/waybar/colors.css
#   pkill -SIGUSR2 waybar && exit
#   echo $MODES
# fi
