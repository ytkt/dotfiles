
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

export PATH="$(brew --prefix)/bin:$PATH"


if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi