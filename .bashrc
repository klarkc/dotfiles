#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
EDITOR=vim

# JavaScript
NPM_PACKAGES="${HOME}/.npm-packages"
YARN_HOME="${HOME}/.yarn"
export PATH="$PATH:$YARN_HOME/bin:$NPM_PACKAGES/bin:${HOME}/.local/bin"

# Android shit
export PATH="$PATH:/opt/android-sdk/tools/bin"
export ANDROID_SDK_ROOT=$HOME/Sources/Android
export ANDROID_HOME=$ANDROID_SDK_ROOT
export JAVA_HOME=/usr/lib/jvm/java-16-openjdk
alias sdkmanager="JAVA_HOME=/opt/android-studio/jre/ sdkmanager --sdk_root=$ANDROID_SDK_ROOT"
alias avdmanager="JAVA_HOME=/opt/android-studio/jre/ avdmanager"
export CAPACITOR_ANDROID_STUDIO_PATH=/opt/android-studio/bin/studio.sh

# Docker
export UID_GID="$(id -u):$(id -g)"

# Color for less and man 
export MANPAGER='less -s -M +Gg'
export LESS="--RAW-CONTROL-CHARS"
lesscolors=$HOME/.LESS_TERMCAP
[[ -f $lesscolors ]] && . $lesscolors

# Tmux
export TMUX_PLUGIN_MANAGER_PATH=$HOME/.tmux/plugins/tpm

