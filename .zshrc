fpath=($(brew --prefix)/share/zsh-completions $fpath)

autoload -U compinit
compinit -u

. $HOME/.zsh.zplug


#export PS1='╭─[\[\033[32m\]\u@\h\[\033[00m\]:\[\033[34m\]\w\[\033[00m\]]\[\033[31m\]\[\033[00m\]\n╰─○ '
NEWLINE=$'\n'
autoload -Uz vcs_info
setopt prompt_subst
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+"
zstyle ':vcs_info:*' formats "%F{green}%c%u[%b]%f"
zstyle ':vcs_info:*' actionformats '[%b|%a]'
precmd () { vcs_info }
PROMPT="╭─[%F{green}${USER}@${HOST}%f]${vcs_info_msg_0_}${NEWLINE}╰─○ "
RPROMPT="%{${fg[blue]}%}[%~]%{${reset_color}%}"


alias emacs='emacsclient -nw -a ""'
alias pwdc='pwd | pbcopy'
alias mindent='gnuindent -nbad -bap -nbc -nbbo -br -ce -cdw -cdb -sc -cs -di2 -ndj -i2 -nip -lp -npcs -nprs -psl -nsaf -nsai -nsaw -nsob'
alias javac='javac -J-Dfile.encoding=UTF-8'
alias java='java -Dfile.encoding=UTF-8'
alias l='ls -l'
alias la='ls -al'
#alias start_psql='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
#alias stop_psql='pg_ctl -D /usr/local/var/postgres stop -s -m fast'

# heroku
export PATH="/usr/local/heroku/bin:$PATH"
