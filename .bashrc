# Bash initialization for interactive non-login shells and
# for remote shells (info "(bash) Bash Startup Files").

# Export 'SHELL' to child processes.  Programs such as 'screen'
# honor it and otherwise use /bin/sh.
export SHELL

if [[ $- != *i* ]]
then
    # We are being invoked from a non-interactive shell.  If this
    # is an SSH session (as in "ssh host command"), source
    # /etc/profile so we get PATH and other essential variables.
    [[ -n "$SSH_CLIENT" ]] && source /etc/profile

    # Don't do anything else.
    return
fi

# Source the system-wide file.
source /etc/bashrc

# mkcd(){
# 	mkdir -p "$@" && cd "$1"
# }
#
# # alias ls='ls -p --color=auto'
# # alias ll='ls -lh'
# # alias la='ls -lAh'
# # alias l='ls -1'
# # alias grep='grep --color=auto'
#
# # cd into directory by typing directory name
# source /etc/bashrc
#
# shopt -s autocd
# # parse_git_branch() {
# #     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
# # }

# Updated PS1 with newline and git branch
# export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 2)\]\$(parse_git_branch)\[$(tput setaf 1)\]]\n\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"

# v1
export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\n\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"

# export PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"

if [[ "$TERM_PROGRAM" == "ghostty" ]]; then
    export TERM=xterm-256color
fi

[[ "${TERM}" == dumb ]] && PS1='$ ' && return

# load aliases
[ -f "$HOME/.config/aliasrc" ] && source "$HOME/.config/aliasrc"

# NPM_PACKAGES="${HOME}/.npm-packages"
# export PATH="$PATH:$NPM_PACKAGES/bin"
# export MANPATH="${MANPATH-$(manpath)}:$NPM_PACKAGES/share/man"
# # add Pulumi to the PATH
# export PATH=$PATH:/home/boogs/.pulumi/bin
# export PATH=$PATH:/home/boogs/.guix-profile/lib
# export LD_LIBRARY_PATH=$LIBRARY_PATH
