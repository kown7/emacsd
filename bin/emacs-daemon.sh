#!/bin/bash
FILENAME=~/.emacs.d/.emacs.desktop.lock
PSAUX=$(ps aux)

if [[ -f ${FILENAME} ]]; then
	echo $PSAUX | grep "emacs --daemon"
	if [[ "$?" != "0" ]]; then
		echo "Removing lock-file: ${FILENAME}"
		ls -lh ~/.emacs.d/.emacs.desktop.lock
		rm ~/.emacs.d/.emacs.desktop.lock
	else
		echo "Shutdown emacs:"
		echo "emacsclient -ne '(save-buffers-kill-emacs)'"
	fi
	echo "Run script again to start daemon"
else
	echo "Starting emacs"
	emacs --daemon
fi

