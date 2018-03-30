#!/bin/sh

[ -f /etc/xprofile ] && source /etc/xprofile
[ -f ~/.xprofile ] && source ~/.xprofile

# set key repeate rate/speed
xset r rate 220 40

exec xmonad
