# .bashrc

# -- Improved X11 forwarding through GNU Screen (or tmux).
# If not in screen or tmux, update the DISPLAY cache.
# If we are, update the value of DISPLAY to be that in the cache.
function update-x11-forwarding
{
  TDN=`tmux display-message -p '#S'`
  NOW=`date`
  # The following line fills up the log quickly:
  #echo "$NOW : $TDN" >> ~/.display.log
  if [ "left" = "$TDN" ]; then 
    export DISPLAY=`cat ~/.displayLeft.txt`
  fi    
  if [ "right" = "$TDN" ]; then 
    export DISPLAY=`cat ~/.displayRight.txt`
  fi
}


#################################################################
# Source global definitions
#################################################################
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

if [ -f /etc/bash.bashrc ]; then
	. /etc/bash.bashrc
fi

# Source aliases
if [ -f ~/.bash_aliases ]; then
	. ~/.bash_aliases
fi

if [ -f ~/.bash_local ]; then
	. ~/.bash_local
fi

# Source aliases
if [ -f ~/.bash_prompt ]; then
	. ~/.bash_prompt
fi

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize


#################################################################
# This is run before every command.
#################################################################
source ~/.bash_preexec
preexec() {
  # Don't cause a preexec for PROMPT_COMMAND.
  # Beware!  This fails if PROMPT_COMMAND is a string containing more than one command.
  [ "$BASH_COMMAND" = "$PROMPT_COMMAND" ] && return

  # Debugging.
  #echo DISPLAY = $DISPLAY, display.txt = `cat ~/.display.txt`, STY = $STY, TMUX = $TMUX
}

update-x11-forwarding
preexec_install

if [ "$(grep -c microsoft /proc/version)" != "0" ]; then
	export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0
	export LIBGL_ALWAYS_INDIRECT=1
	umask 022
fi;
