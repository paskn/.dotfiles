#!/bin/bash

# 1. Get a list of all VPN connections
vpns=$(nmcli -g NAME,TYPE connection show | grep ':vpn' | cut -d: -f1)

# 2. Get the list of currently active connections
active_vpn=$(nmcli -g NAME,TYPE connection show --active | grep ':vpn' | cut -d: -f1)

# 3. Format the list for Rofi
rofi_list=""
for vpn in $vpns; do
    if [[ "$vpn" == "$active_vpn" ]]; then
        # Mark active with an asterisk
        rofi_list+="* $vpn\n"
    else
        rofi_list+="$vpn\n"
    fi
done

# 4. Show the Rofi menu
selected=$(echo -e "$rofi_list" | rofi -dmenu -p "VPN" -i -no-custom)

# 5. Handle the user's choice
if [ -n "$selected" ]; then
    # Clean the "* " prefix to get the real connection name for nmcli
    clean_name=$(echo "$selected" | sed 's/\* //')

    # FIX IS HERE: Check if the selected string contains the asterisk
    if [[ "$selected" == *"* "* ]]; then
        # If the user selected the active one (marked with *), disconnect it
        nmcli connection down "$clean_name"
        notify-send "VPN" "$clean_name Disconnected"
    else
        # If they selected a different one, connect it
        nmcli connection up "$clean_name"
        notify-send "VPN" "Connected to $clean_name"
    fi
fi
