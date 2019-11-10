fpath=($(brew --prefix)/share/zsh-completions $fpath)

autoload -U compinit
compinit -u

. $HOME/.zsh.zplug

HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data

#export PS1='╭─[\[\033[32m\]\u@\h\[\033[00m\]:\[\033[34m\]\w\[\033[00m\]]\[\033[31m\]\[\033[00m\]\n╰─○ '
NEWLINE=$'\n'
autoload -Uz vcs_info
setopt prompt_subst

zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{yellow}!"
zstyle ':vcs_info:git:*' unstagedstr "%F{red}+"
zstyle ':vcs_info:*' formats "%F{green}%c%u[%b]%f"
zstyle ':vcs_info:*' actionformats '(%b|%a)'
precmd () { vcs_info }
precmd () { vcs_info }
PROMPT='╭─[%F{green}${USER}@${HOST}%f]${vcs_info_msg_0_}${NEWLINE}╰─○ '
RPROMPT='%{${fg[blue]}%}[%~ %T]%{${reset_color}%}'


alias emacs='emacsclient -nw -a ""'
alias pwdc='pwd | pbcopy'
alias mindent='gnuindent -nbad -bap -nbc -nbbo -br -ce -cdw -cdb -sc -cs -di2 -ndj -i2 -nip -lp -npcs -nprs -psl -nsaf -nsai -nsaw -nsob'
alias javac='javac -J-Dfile.encoding=UTF-8'
alias java='java -Dfile.encoding=UTF-8'
alias l='ls -l'
alias la='ls -al'
#alias start_psql='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
#alias stop_psql='pg_ctl -D /usr/local/var/postgres stop -s -m fast'
alias be="bundle exec"

#eval `keychain --inherit any --eval --agents ssh,gpg github_rsa A76FA1B3 --timeout 3000`
eval `keychain --inherit any --eval --agents ssh github_rsa --timeout 3000`

# heroku
export PATH="/usr/local/heroku/bin:$PATH"
eval "$(rbenv init -)"

# elasticbeanstalk
export PATH="~/Library/Python/2.7/bin:$PATH"

# nvm
export NVM_DIR="$HOME/.nvm"
. "$(brew --prefix nvm)/nvm.sh"
. "/usr/local/opt/nvm/nvm.sh"

autoload -U compinit
compinit
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
alias ls="ls -GF"
alias gls="gls --color"
zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

# The next line updates PATH for the Google Cloud SDK.
source "$HOME/dev/google-cloud-sdk/path.zsh.inc"

# The next line enables shell command completion for gcloud.
source "$HOME/dev/google-cloud-sdk/completion.zsh.inc"

setopt nonomatch

# openssl
export PATH="/usr/local/opt/openssl/bin:$PATH"

export PATH="$PATH:$HOME/dev/lib/flutter/bin"
