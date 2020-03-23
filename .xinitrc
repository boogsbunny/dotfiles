#!/usr/bin/env sh

[ -f "$HOME/.xprofile" ] && . ~/.xprofile
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

exec dwm
