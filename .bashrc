# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Vi like shortcuts
set -o vi

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
export EDITOR=vim

# JavaScript
NPM_PACKAGES="${HOME}/.npm-packages"
YARN_HOME="${HOME}/.yarn"
export PATH="$PATH:$YARN_HOME/bin:$NPM_PACKAGES/bin:${HOME}/.local/bin"

# Color for less and man 
export MANPAGER='less -s -M +Gg'
export LESS="--RAW-CONTROL-CHARS"
lesscolors=$HOME/.LESS_TERMCAP
[[ -f $lesscolors ]] && . $lesscolors

# QT
export QT_QPA_PLATFORMTHEME=gtk2

# Direnv hook
eval "$(direnv hook bash)"

# Tmux
export TMUX_PLUGIN_MANAGER_PATH=$HOME/.tmux/plugins/tpm
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  exec tmux
fi

