# for MacPorts
#export PATH=/opt/local/bin:opt/local/sbin/:$PATH

# for Homebrew
export PATH=/usr/local/bin:$PATH


export MANPATH=/opt/local/man:$MANPATH

# pkgconfig's path
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig/


# carbon emacs
# alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'

# Cocoa Emacs
#alias emacs='open -n /Applications/Emacs.app'
alias emacsnw='emacs -nw'
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
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

# RVM
[ -s ${HOME}/.rvm/scripts/rvm ] && source ${HOME}/.rvm/scripts/rvm

# php (homebrew)
if [ -f -/.bashrc ] ; then
  . ~/.bashrc
fi


# libly production
#export FB_APP_KEY='356730561081810'
#export FB_SECRET_KEY='c803b5fc37120b91920cd2ef6ded32e2'

# libly staging
export FB_APP_KEY='126393170841327'
export FB_SECRET_KEY='1afa51aea389e67cc04791e0af67cb5c'
### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
