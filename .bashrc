# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# set a fancy git prompt (non-color, unless we know we "want" color)
source /usr/share/git/completion/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export GIT_PS1_SHOWUPSTREAM="auto"
case "$TERM" in
  xterm-color|xterm|xterm-256color|tmux-256color)
    export PROMPT_COMMAND='__git_ps1 "\u@\h \w" "\\\$ "'
    export GIT_PS1_SHOWCOLORHINTS=true
    ;;
  *)
    export PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
    ;;
esac

# Vi like shortcuts
set -o vi

# Editor
export EDITOR=vim

# Lsd
alias lsd="lsd --config-file ${HOME}/.lsd.yml"
alias ls='lsd'
alias l='ls -l'
alias ll='ls -la'
alias lt='ls --tree'

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

# Override bashrc
override="${HOME}/.bashrc_override" 
[[ -f $override ]] && . $override 
