# ~/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# set a fancy git prompt (non-color, unless we know we "want" color)
source /usr/share/git/completion/git-prompt.sh
export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWUNTRACKEDFILES=true
export GIT_PS1_SHOWUPSTREAM="auto"
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  UHOST="\u@\h"
fi
case "$TERM" in
  xterm-color|xterm|xterm-256color|tmux-256color)
    export PROMPT_COMMAND="__git_ps1 \"${UHOST} \w\" \"$ \""
    export GIT_PS1_SHOWCOLORHINTS=true
    ;;
  *)
    export PS1="[${UHOST} \W\$(__git_ps1 \" (%s)\")]\$ "
    ;;
esac
unset UHOST

# increase history size
export HISTSIZE=10000
export HISTFILESIZE=10000

# store current history when opening a shell to avoid losses
PROMPT_COMMAND="${PROMPT_COMMAND} && history -a"

# Vi like shortcuts
set -o vi

# Editor
export EDITOR=vim

# Lsd
alias lsd="lsd --config-file ${HOME}/.lsd.yml"
alias ls='lsd'
alias l='ls'
alias ll='ls -la'
alias lt='ls --tree'

# yay
alias yay="yay --noconfirm"

# Dircolors
dir_colors="${HOME}/.local/bin/dir_colors"
test -r $dir_colors && eval $(dircolors $dir_colors)

# Grep
case "$TERM" in
  xterm-color|xterm|xterm-256color|tmux-256color)
    alias grep="grep --color"
    ;;
esac

# JavaScript
NPM_PACKAGES="${HOME}/.npm-packages"
YARN_HOME="${HOME}/.yarn"
export PATH="$PATH:$YARN_HOME/bin:$NPM_PACKAGES/bin:${HOME}/.local/bin"

# PureScript
alias setpurs="nix develop github:justinwoo/easy-purescript-nix#deluxe"

# Color for less and man 
export MANPAGER='less -s -M +Gg'
export LESS="--RAW-CONTROL-CHARS"
lesscolors=$HOME/.LESS_TERMCAP
[[ -f $lesscolors ]] && . $lesscolors

# QT
export QT_QPA_PLATFORMTHEME=gtk2
export QT_STYLE_OVERRIDE=gtk2

# Direnv hook
eval "$(direnv hook bash)"

# Tmux
export TMUX_PLUGIN_MANAGER_PATH=$HOME/.tmux/plugins/tpm
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  exec tmux
fi

# Load nix profile.d
if [ -d "${HOME}/.nix-profile/etc/profile.d" ]; then
  for i in "${HOME}/.nix-profile/etc/profile.d/"*.sh; do
    if [ -r "$i" ]; then
      . "$i"
    fi
  done
fi

# Override bashrc
override="${HOME}/.bashrc_override" 
[[ -f $override ]] && . $override 


export GPG_TTY=$(tty)

# Shell-GPT integration BASH v0.2
_sgpt_bash() {
if [[ -n "$READLINE_LINE" ]]; then
    READLINE_LINE=$(sgpt --shell --role "phi3-shell" <<< "$READLINE_LINE" --no-interaction)
    READLINE_POINT=${#READLINE_LINE}
fi
}
bind -x '"\C-e": _sgpt_bash'
_sgpt_bash_repl() {
    true || rm /tmp/chat_cache/shell
    sgpt --repl shell --shell
}
bind -x '"\C-o": _sgpt_bash_repl'
# Shell-GPT integration BASH v0.2
