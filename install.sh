#!/bin/bash

pip3 install jedi rope flake8 autopep8 yapf black ipython pyflake

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

ln -s .dotfiles/gtags.conf .globalrc
ln -s $HOME/.dotfiles/bin/backup-borg.sh $HOME/bin/backup-borg.sh
