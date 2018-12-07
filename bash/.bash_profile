#!/bin/bash
# This is for login-shells only
export PATH=$HOME/bin:$PATH
export DO_COLOR=1
export DISPLAY=:0 # this is for WSL only

if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

### This Changes The PS1 ### {{{
export PROMPT_COMMAND=__prompt_command      # Func to gen PS1 after CMDs


