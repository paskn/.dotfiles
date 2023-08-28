#!/bin/zsh

print "#--- Xcode Dev Tools ---#"
if test ! $(which xcode-select); then
    print 'Installing Xcode Dev Tools' ;
    xcode-select --install
else
    print 'Xcode Dev Tools is already installed!';
fi

# Check for Homebrew,
# Install if we don't have it
if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# ----------------------------------------------------------
# ---------Disable Homebrew user behavior analytics---------
# ----------------------------------------------------------
echo '--- Disable Homebrew user behavior analytics'
command='export HOMEBREW_NO_ANALYTICS=1'
declare -a profile_files=("$HOME/.bash_profile" "$HOME/.zprofile")
for profile_file in "${profile_files[@]}"
do
    touch "$profile_file"
    if ! grep -q "$command" "${profile_file}"; then
        echo "$command" >> "$profile_file"
        echo "[$profile_file] Configured"
    else
        echo "[$profile_file] No need for any action, already configured"
    fi
done
# ----------------------------------------------------------


# Make sure we’re using the latest Homebrew.
brew update

# Upgrade any already-installed formulae.
brew upgrade --all

# Require password immediately after sleep or screen saver begins
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 0

# Enable repeating keys
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Disable the “Are you sure you want to open this application?” dialog
defaults write com.apple.LaunchServices LSQuarantine -bool false

# Remove workspace auto-switching
defaults write com.apple.dock workspaces-auto-swoosh -bool NO
killall Dock # Restart the Dock process

# Save screenshots to the Desktop/Screenshots
mkdir ${HOME}/Desktop/Screenshots
defaults write com.apple.screencapture location -string "${HOME}/Desktop/Screenshots"

# Finder: Show files extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Finder: Use list view
defaults write com.apple.finder FXPreferredViewStyle Nlsv

# Finder: Stop creating .DS_Store
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Finder: Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Finder: Show the ~/Library folder
chflags nohidden ~/Library

# Finder: disable window animations and Get Info animations
defaults write com.apple.finder DisableAllAnimations -bool true

# Finder: Display full POSIX path as Finder window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# Finder: allow text selection in Quick Look
defaults write com.apple.finder QLEnableTextSelection -bool true

# Finder: When performing a search, search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Set the icon size of Dock items to 36 pixels
defaults write com.apple.dock tilesize -int 36

# Don’t automatically rearrange Spaces based on most recent use
defaults write com.apple.dock mru-spaces -bool false

# Copy email addresses as `foo@example.com` instead of `Foo Bar <foo@example.com>` in Mail.app
defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false

# Disable smart quotes as they’re annoying when typing code
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# Disable smart dashes as they’re annoying when typing code
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# Trackpad: enable tap to click for this user and for the login screen
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Enable full keyboard access for all controls
# (e.g. enable Tab in modal dialogs)
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# Set a blazingly fast keyboard repeat rate
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 15

# The following is from https://privacy.sexy/

# ----------------------------------------------------------
# ----Clear File Quarantine logs of all downloaded files----
# ----------------------------------------------------------
echo '--- Clear File Quarantine logs of all downloaded files'
db_file=~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV2
db_query='delete from LSQuarantineEvent'
if [ -f "$db_file" ]; then
    echo "Database exists at \"$db_file\""
    if ls -lO "$db_file" | grep --silent 'schg'; then
        sudo chflags noschg "$db_file"
        echo "Found and removed system immutable flag"
        has_sytem_immutable_flag=true
    fi
    if ls -lO "$db_file" | grep --silent 'uchg'; then
        sudo chflags nouchg "$db_file"
        echo "Found and removed user immutable flag"
        has_user_immutable_flag=true
    fi
    sqlite3 "$db_file" "$db_query"
    echo "Executed the query \"$db_query\""
    if [ "$has_sytem_immutable_flag" = true ] ; then
        sudo chflags schg "$db_file"
        echo "Added system immutable flag back"
    fi
    if [ "$has_user_immutable_flag" = true ] ; then
        sudo chflags uchg "$db_file"
        echo "Added user immutable flag back"
    fi
else
    echo "No action needed, database does not exist at \"$db_file\""
fi
# ----------------------------------------------------------


# Disable Personalized advertisements and identifier collection
echo '--- Disable Personalized advertisements and identifier collection'
defaults write com.apple.AdLib allowIdentifierForAdvertising -bool false
defaults write com.apple.AdLib allowApplePersonalizedAdvertising -bool false
defaults write com.apple.AdLib forceLimitAdTracking -bool true
# ----------------------------------------------------------


# ----------------------------------------------------------
# --Clear File Quarantine attribute from downloaded files---
# ----------------------------------------------------------
echo '--- Clear File Quarantine attribute from downloaded files'
find ~/Downloads        \
        -type f         \
        -exec           \
            sh -c       \
                '
                    attr="com.apple.quarantine"
                    file="{}"
                    if [[ $(xattr "$file") = *$attr* ]]; then
                        if xattr -d "$attr" "$file" 2>/dev/null; then
                            echo "🧹 Cleaned attribute from \"$file\""
                        else
                            >&2 echo "❌ Failed to clean attribute from \"$file\""
                        fi
                    else
                        echo "No attribute in \"$file\""
                    fi
                '       \
            {} \;
# ----------------------------------------------------------


# ----------------------------------------------------------
# -----Prevent quarantine from logging downloaded files-----
# ----------------------------------------------------------
echo '--- Prevent quarantine from logging downloaded files'
file_to_lock=~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV2
if [ -f "$file_to_lock" ]; then
    sudo chflags schg "$file_to_lock"
    echo "Made file immutable at \"$file_to_lock\""
else
    echo "No action is needed, file does not exist at \"$file_to_lock\""
fi
# ----------------------------------------------------------


# Disable using extended quarantine attribute on downloaded files (disables warning)
echo '--- Disable using extended quarantine attribute on downloaded files (disables warning)'
sudo defaults write com.apple.LaunchServices 'LSQuarantine' -bool NO
# ----------------------------------------------------------


# ----------------------------------------------------------
# ---------------Disable Remote Apple Events----------------
# ----------------------------------------------------------
echo '--- Disable Remote Apple Events'
sudo systemsetup -setremoteappleevents off
# ----------------------------------------------------------


# ----------------------------------------------------------
# ------------Opt-out from Siri data collection-------------
# ----------------------------------------------------------
echo '--- Opt-out from Siri data collection'
defaults write com.apple.assistant.support 'Siri Data Sharing Opt-In Status' -int 2
# ----------------------------------------------------------


# ----------------------------------------------------------
# --------------------Disable "Ask Siri"--------------------
# ----------------------------------------------------------
echo '--- Disable "Ask Siri"'
defaults write com.apple.assistant.support 'Assistant Enabled' -bool false
# ----------------------------------------------------------


# ----------------------------------------------------------
# ---------------Disable Siri voice feedback----------------
# ----------------------------------------------------------
echo '--- Disable Siri voice feedback'
defaults write com.apple.assistant.backedup 'Use device speaker for TTS' -int 3
# ----------------------------------------------------------


# ----------------------------------------------------------
# -------Disable Siri services (Siri and assistantd)--------
# ----------------------------------------------------------
echo '--- Disable Siri services (Siri and assistantd)'
launchctl disable "user/$UID/com.apple.assistantd"
launchctl disable "gui/$UID/com.apple.assistantd"
sudo launchctl disable 'system/com.apple.assistantd'
launchctl disable "user/$UID/com.apple.Siri.agent"
launchctl disable "gui/$UID/com.apple.Siri.agent"
sudo launchctl disable 'system/com.apple.Siri.agent'
if [ $(/usr/bin/csrutil status | awk '/status/ {print $5}' | sed 's/\.$//') = "enabled" ]; then
    >&2 echo 'This script requires SIP to be disabled. Read more: https://developer.apple.com/documentation/security/disabling_and_enabling_system_integrity_protection'
fi
# ----------------------------------------------------------


# ----------------------------------------------------------
# -------Disable "Do you want to enable Siri?" pop-up-------
# ----------------------------------------------------------
echo '--- Disable "Do you want to enable Siri?" pop-up'
defaults write com.apple.SetupAssistant 'DidSeeSiriSetup' -bool True
# ----------------------------------------------------------


# ----------------------------------------------------------
# ---------Deactivate the Remote Management Service---------
# ----------------------------------------------------------
echo '--- Deactivate the Remote Management Service'
sudo /System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Resources/kickstart -deactivate -stop
# ----------------------------------------------------------


# ----------------------------------------------------------
# -----------Remove Apple Remote Desktop Settings-----------
# ----------------------------------------------------------
echo '--- Remove Apple Remote Desktop Settings'
sudo rm -rf /var/db/RemoteManagement
sudo defaults delete /Library/Preferences/com.apple.RemoteDesktop.plist
defaults delete ~/Library/Preferences/com.apple.RemoteDesktop.plist
sudo rm -r /Library/Application\ Support/Apple/Remote\ Desktop/ 
rm -r ~/Library/Application\ Support/Remote\ Desktop/
rm -r ~/Library/Containers/com.apple.RemoteDesktop
# ----------------------------------------------------------


# ----------------------------------------------------------
# ------------------Disable Captive portal------------------
# ----------------------------------------------------------
echo '--- Disable Captive portal'
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.captive.control.plist Active -bool false
# ----------------------------------------------------------

# Require a password to wake the computer from sleep or screen saver
echo '--- Require a password to wake the computer from sleep or screen saver'
sudo defaults write /Library/Preferences/com.apple.screensaver askForPassword -bool true
# ----------------------------------------------------------


# Initiate session lock five seconds after screen saver is started
echo '--- Initiate session lock immediately after screen saver is started'
sudo defaults write /Library/Preferences/com.apple.screensaver 'askForPasswordDelay' -int 0
# ----------------------------------------------------------

# ----------------------------------------------------------
# ---------------Enable application firewall----------------
# ----------------------------------------------------------
echo '--- Enable application firewall'
/usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on
sudo defaults write /Library/Preferences/com.apple.alf globalstate -bool true
defaults write com.apple.security.firewall EnableFirewall -bool true
# ----------------------------------------------------------


# ----------------------------------------------------------
# -----------------Turn on firewall logging-----------------
# ----------------------------------------------------------
echo '--- Turn on firewall logging'
/usr/libexec/ApplicationFirewall/socketfilterfw --setloggingmode on
sudo defaults write /Library/Preferences/com.apple.alf loggingenabled -bool true
# ----------------------------------------------------------


# ----------------------------------------------------------
# -------------------Turn on stealth mode-------------------
# ----------------------------------------------------------
echo '--- Turn on stealth mode'
/usr/libexec/ApplicationFirewall/socketfilterfw --setstealthmode on
sudo defaults write /Library/Preferences/com.apple.alf stealthenabled -bool true
defaults write com.apple.security.firewall EnableStealthMode -bool true
# ----------------------------------------------------------


# Prevent automatically allowing incoming connections to signed apps
echo '--- Prevent automatically allowing incoming connections to signed apps'
sudo defaults write /Library/Preferences/com.apple.alf allowsignedenabled -bool false
# ----------------------------------------------------------


# Prevent automatically allowing incoming connections to downloaded signed apps
echo '--- Prevent automatically allowing incoming connections to downloaded signed apps'
sudo defaults write /Library/Preferences/com.apple.alf allowdownloadsignedenabled -bool false
# ----------------------------------------------------------


# ----------------------------------------------------------
# ----Disables signing in as Guest from the login screen----
# ----------------------------------------------------------
echo '--- Disables signing in as Guest from the login screen'
sudo defaults write /Library/Preferences/com.apple.loginwindow GuestEnabled -bool NO
# ----------------------------------------------------------


# ----------------------------------------------------------
# -------Disables Guest access to file shares over AF-------
# ----------------------------------------------------------
echo '--- Disables Guest access to file shares over AF'
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server AllowGuestAccess -bool NO
# ----------------------------------------------------------


# ----------------------------------------------------------
# ------Disables Guest access to file shares over SMB-------
# ----------------------------------------------------------
echo '--- Disables Guest access to file shares over SMB'
sudo defaults write /Library/Preferences/com.apple.AppleFileServer guestAccess -bool NO
# ----------------------------------------------------------

# ----------------------------------------------------------
# --Disable sharing of local printers with other computers--
# ----------------------------------------------------------
echo '--- Disable sharing of local printers with other computers'
cupsctl --no-share-printers
# ----------------------------------------------------------


# ----------------------------------------------------------
# -Disable printing from any address including the Internet-
# ----------------------------------------------------------
echo '--- Disable printing from any address including the Internet'
cupsctl --no-remote-any
# ----------------------------------------------------------


# ----------------------------------------------------------
# ----------Disable remote printer administration-----------
# ----------------------------------------------------------
echo '--- Disable remote printer administration'
cupsctl --no-remote-admin
# ----------------------------------------------------------

# Applications
# ----------------------------------------------------------
# ----Disable Microsoft Office diagnostics data sending-----
# ----------------------------------------------------------
echo '--- Disable Microsoft Office diagnostics data sending'
defaults write com.microsoft.office DiagnosticDataTypePreference -string ZeroDiagnosticData
# ----------------------------------------------------------

# ----------------------------------------------------------
# ----------------Disable Firefox telemetry-----------------
# ----------------------------------------------------------
echo '--- Disable Firefox telemetry'
# Enable Firefox policies so the telemetry can be configured.
sudo defaults write /Library/Preferences/org.mozilla.firefox EnterprisePoliciesEnabled -bool TRUE
# Disable sending usage data
sudo defaults write /Library/Preferences/org.mozilla.firefox DisableTelemetry -bool TRUE
# ----------------------------------------------------------


# ----------------------------------------------------------
# -----------------Uninstall Google update------------------
# ----------------------------------------------------------
echo '--- Uninstall Google update'
googleUpdateFile=~/Library/Google/GoogleSoftwareUpdate/GoogleSoftwareUpdate.bundle/Contents/Resources/ksinstall
if [ -f "$googleUpdateFile" ]; then
    $googleUpdateFile --nuke
    echo Uninstalled google update
else
    echo Google update file does not exist
fi
# ----------------------------------------------------------

# ----------------------------------------------------------
# --------------Disable NET Core CLI telemetry--------------
# ----------------------------------------------------------
echo '--- Disable NET Core CLI telemetry'
command='export DOTNET_CLI_TELEMETRY_OPTOUT=1'
declare -a profile_files=("$HOME/.bash_profile" "$HOME/.zprofile")
for profile_file in "${profile_files[@]}"
do
    touch "$profile_file"
    if ! grep -q "$command" "${profile_file}"; then
        echo "$command" >> "$profile_file"
        echo "[$profile_file] Configured"
    else
        echo "[$profile_file] No need for any action, already configured"
    fi
done
# ----------------------------------------------------------


# ----------------------------------------------------------
# ------------Disable PowerShell Core telemetry-------------
# ----------------------------------------------------------
echo '--- Disable PowerShell Core telemetry'
command='export POWERSHELL_TELEMETRY_OPTOUT=1'
declare -a profile_files=("$HOME/.bash_profile" "$HOME/.zprofile")
for profile_file in "${profile_files[@]}"
do
    touch "$profile_file"
    if ! grep -q "$command" "${profile_file}"; then
        echo "$command" >> "$profile_file"
        echo "[$profile_file] Configured"
    else
        echo "[$profile_file] No need for any action, already configured"
    fi
done
# ----------------------------------------------------------
