export CHATTY_TWITCH_TOKEN=$(twitch token 2>&1 | awk -F': ' '{ print $2 }')

