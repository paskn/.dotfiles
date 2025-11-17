#!/bin/bash

# Pull out a menu with scripts in ~/bin

directory=~/bin/

selected=$(ls $directory|rofi -dmenu -p "Run")
/bin/bash $directory/$selected
