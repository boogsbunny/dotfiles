#!/bin/sh

appendpath () {
	[ $# -eq 2 ] && PATHVAR=$2 || PATHVAR=PATH
	[ -d "$1" ] || return
	eval echo \$$PATHVAR | grep -q "\(:\|^\)$1\(:\|$\)" && return
	eval export $PATHVAR="\$$PATHVAR:$1"
}

appendpath "${HOME}/.local/bin"
appendpath "${HOME}/.local/bin/statusbar"
appendpath "${HOME}/go/bin"
appendpath "${HOME}/go/bin/gopls"
appendpath "${HOME}/.npm-packages/bin"

appendxdgdata () {
	[ $# -eq 2 ] && PATHVAR=$2 || PATHVAR=XDG_DATA_DIRS
	[ -d "$1" ] || return
	eval echo \$$PATHVAR | grep -q "\(:\|^\)$1\(:\|$\)" && return
	eval export $PATHVAR="\$$PATHVAR:$1"
}

appendxdgdata "${HOME}/.local/share/flatpak/exports/share"
export XDG_DATA_DIRS=/var/lib/flatpak/exports/share:$XDG_DATA_DIRS

for i in emacsclient em emacs vim nano vi; do
	command -v $i >/dev/null 2>&1 && export EDITOR=$i && break
done
GIT_EDITOR="$EDITOR"
VISUAL="$EDITOR"
[ "$GIT_EDITOR" = em ] && GIT_EDITOR=emc
[ "$VISUAL" = emc ] && VISUAL=emc
export GIT_EDITOR
export VISUAL
export TERMINAL="screen-256color"
export BROWSER="nyxt"
export TERM="xterm"
# export STATUSBAR="dwmblocks"
export READER="zathura"
# export FILE="ranger"
export MANWIDTH=70
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5
# export GDK_SCALE=1.0
# export GDK_DPI_SCALE=1.0
# export STATUSBARSIZE=25
# export FONTSIZE=18
export STATUSBARSIZE=14
export FONTSIZE=18
LESSHISTFILE='-'
export GOPATH="$HOME/go"
# export GOROOT="$HOME/go"
export PERSONAL="$HOME/projects/personal"
export SBCL_HOME="$HOME/.guix-profile/lib/sbcl"
export NODE_PATH="$HOME/.npm-packages/lib/node_modules"
export PATH=$PATH:/home/boogs/.guix-profile/lib
export LD_LIBRARY_PATH=$LIBRARY_PATH

## SSH-Agent
## Set SSH to use gpg-agent
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi
# Set GPG TTY
export GPG_TTY=$(tty)
# Refresh gpg-agent tty in case user switches into an X session
gpg-connect-agent updatestartuptty /bye >/dev/null

## Guix
export GUIX_DISTRO_AGE_WARNING=1m
for i in ~/.guix-extra-profiles/*; do
 	profile=$i/$(basename "$i")
 	if [ -f "$profile"/etc/profile ]; then
 		GUIX_PROFILE="$profile" ; . "$profile"/etc/profile
		export MANPATH="$profile"/share/man:$MANPATH
		export INFOPATH="$profile"/share/info:$INFOPATH
		export XDG_DATA_DIRS="$profile"/share:$XDG_DATA_DIRS
		export XDG_CONFIG_DIRS="$profile"/etc/xdg:$XDG_CONFIG_DIRS
 	fi
 	unset profile
done

echo "$0" | grep "bash$" >/dev/null && [ -f ~/.bashrc ] && source "$HOME/.bashrc"

[ "$(tty)" = "/dev/tty1" ] && ! pgrep -x i3 >/dev/null && exec startx

shepherd
