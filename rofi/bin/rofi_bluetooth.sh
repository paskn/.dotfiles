#!/bin/bash

# toggle connection to known bluetooth devices

selected=$(bluetoothctl devices | rofi -dmenu -p "Toggle device" | awk '{print $2}')

status=$(bluetoothctl devices Connected | awk '{print $2}')

echo $status

if [ $status = $selected ]; then
    bluetoothctl disconnect $selected
else
    bluetoothctl connect $selected
fi
