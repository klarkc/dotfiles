# In ~/.bash_profile
# Load generic login settings (including private values) first.
if [ -f ~/.profile ]; then
  . ~/.profile
fi

# Interactive Bash then selects English without changing the desktop locale.
if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi
