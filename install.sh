#!/bin/bash

set -e
# set python3 in Makefile for Pymacs
grep "PYTHON = python3" emacs/Pymacs/Makefile
set +e
echo $(cd emacs/Pymacs && make && python3 setup.py install --user)
pip3 install ropemode rope ropemacs

cd $HOME
ln -s .dotfiles/emacs/ .emacs.d
ln -s .dotfiles/emacs/.emacs .emacs

ln -s .dotfiles/bash/.bash_aliases
ln -s .dotfiles/bash/.bash_preexec
ln -s .dotfiles/bash/.bash_profile
ln -s .dotfiles/bash/.bash_prompt
ln -s .dotfiles/bash/.bashrc

ln -s .dotfiles/tmux/.tmux.conf
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

