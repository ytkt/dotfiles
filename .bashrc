# for Homebrew
export PATH=/usr/local/bin:$PATH


# Add RVM to PATH for scripting
PATH=$PATH:$HOME/.rvm/bin 

export PATH="$(brew --prefix)/bin:$PATH"

export MANPATH=/opt/local/man:$MANPATH

# pkgconfig's path
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig/


# carbon emacs
# alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'

# Cocoa Emacs
#alias emacs='open -n /Applications/Emacs.app'
alias emacsnw='emacs -nw'
alias emacs='/usr/local/Cellar/emacs/24.2/bin/emacs'
#alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
#alias emacs='open -n /Applications/Emacs.app'

# copy current directory path
alias pwdc='pwd | pbcopy'

# 
alias mindent='gnuindent -nbad -bap -nbc -nbbo -br -ce -cdw -cdb -sc -cs -di2 -ndj -i2 -nip -lp -npcs -nprs -psl -nsaf -nsai -nsaw -nsob'

# For javac encoding
alias javac='javac -J-Dfile.encoding=UTF-8'
alias java='java -Dfile.encoding=UTF-8'

# ls comands
alias l='ls -l'
alias la='ls -al'

alias start_psql='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias stop_psql='pg_ctl -D /usr/local/var/postgres stop -s -m fast'
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# heroku
export PATH="/usr/local/heroku/bin:$PATH"

# RVM
[ -s ${HOME}/.rvm/scripts/rvm ] && source ${HOME}/.rvm/scripts/rvm

# php (homebrew)
#if [ -f -/.bashrc ] ; then
#  . ~/.bashrc
#fi

# git settings
source ${HOME}/dotfiles/.bash/git-prompt.sh
source ${HOME}/dotfiles/.bash/git-completion.bash
GIT_PS1_SHOWDIRTYSTATE=true
export PS1='[\[\033[32m\]\u@\h\[\033[00m\]:\[\033[34m\]\w\[\033[00m\]]\[\033[31m\]$(__git_ps1)\[\033[00m\]\n\$ '


# bash color
export LSCOLORS=exfxcxdxbxegedabagacad
alias ls="ls -G"


alias ipa="ifconfig | grep inet"
