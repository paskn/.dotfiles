#!/usr/bin/bash

# Rofi menu to interactively kill a process

selected=$(ps -eo pid,comm | rofi -dmenu -p "Kill" | awk '{print $1 }')

kill -9 $selected

