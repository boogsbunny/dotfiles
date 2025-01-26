#!/bin/sh

if [ $# -eq 0 ]; then
    emacs --daemon --eval "(require 'exwm)" -f exwm-enable
    exec emacsclient -c
fi

exec "$@"
