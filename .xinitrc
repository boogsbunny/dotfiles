#!/usr/bin/env sh

[ -f "$HOME/.xprofile" ] && . ~/.xprofile
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

if [ $# -eq 0 ]; then
	## With no argument, launch custom session.
	conky | dzen2 -h "50" -p -dock -ta l -fn "Fira Mono-$FONTSIZE" &
	## `exwm-enable' has to be called before the frame is spawned.
	emacs --daemon --eval "(require 'exwm)" -f exwm-enable
	exec emacsclient -c
fi

exec "$@"
