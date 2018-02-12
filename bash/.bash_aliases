# User specific aliases and functions
alias gaddu='git add -u :/'
alias gits='git status -uno'
alias gitsa='git status'
alias gca='git commit --amend --no-edit'
alias gfa='git fetch --all'
alias gk='gitk --all  &'

alias c..="cd .."
alias c...="cd ../.."
alias c....="cd ../../.."
alias c.....="cd ../../../.."
alias c......="cd ../../../../.."

alias tma='tmux attach-session'
#alias tml='tmux attach -t left'
#alias tmr='tmux attach -t 1'

# ble color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias lh='ls -lh'

export EDITOR=vim

alias goto_rmtoo='cd ~/workspace/rmtoo/'

