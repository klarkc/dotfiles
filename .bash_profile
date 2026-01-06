# In ~/.bash_profile
if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

# Load generic login-shell settings.
if [ -f ~/.profile ]; then
  . ~/.profile
fi
