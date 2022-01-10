# User specific aliases and functions
alias gaddu='git add -u :/'
alias gits='git status -uno'
alias gitsa='git status'
alias gca='git commit --amend --no-edit'
alias gfa='git fetch --all'
alias gk='gitk --all  &'
alias githeadshort='git rev-parse --short HEAD'

alias c.="cd .."
alias c..="cd ../.."
alias c...="cd ../../.."
alias c....="cd ../../../.."
alias c.....="cd ../../../../.."

alias tma='tmux attach-session'
#alias tml='tmux attach -t left'
#alias tmr='tmux attach -t 1'

alias bel='echo -ne "\a"'
alias cwdiff="wdiff -n -w $'\033[30;41m' -x $'\033[0m' -y $'\033[30;42m' -z $'\033[0m'"

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
alias lst='ls -lrt --color | tail'

export HADES6='fe80::21b:21ff:febd:80cb%eth0'
export EDITOR=vim
alias evim='TERM=xterm-256color emacs -nw'

alias goto_home='cd /var/home/knordstroem'
alias goto_workspace='cd /var/home/knordstroem/workspace'
alias goto_rmtoo='cd ~/workspace/rmtoo/ && tmux rename-window rmtoo'
alias goto_tfml='cd ~/workspace/testframework_ml605/fw/'
alias goto_stetst='cd ~/workspace/fw_stellteil_basis/fw_hwt'

alias wanip6='dig @resolver1.opendns.com A myip.opendns.com +short -6'
alias wanip4='dig @resolver1.opendns.com A myip.opendns.com +short -4'
