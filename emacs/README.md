---
title: Emacs and VHDL
---

# Motivation

While the default VHDL Emacs mode is quite nice, there is room
for improvement. This repository should provide an entry point 
with a working example.

# Installation

The packages for *ctags* and *gtags* are rather outdated on Ubuntu,
hence they need to be installed manually.

## Preparation

```shell
apt install dh-autoreconf pkg-config texinfo
```

## ctags

```shell
git clone https://github.com/universal-ctags/ctags.git
cd ctags
./autogen.sh
./configure
make
sudo make install
# Update ctags to your local installation
sudo update-alternatives --install /usr/bin/ctags ctags /usr/local/bin/ctags 1
```

## global

This package provides gtags et c.

```shell
./configure --with-universal-ctags=/usr/local/bin/ctags
make
sudo make install
```


# Usage

**TODO** parameters in the next paragraph needs to be set in emacs or your
bash settings!

## Create Databases

Derived from [^1], run the following command in the root directory of
your project.

```
GTAGSLABEL=ctags GTAGSCONF=/usr/local/share/gtags/gtags.conf gtags
```

## Update Databases

From [^2], the following updates the database. However, the helm
package should have taken care thereof (TBC).

```lisp
    (defun gtags-update-single(filename)  
      "Update Gtags database for changes in a single file"
      (interactive)
      (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

    (defun gtags-update-current-file()
      (interactive)
      (defvar filename)
      (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
      (gtags-update-single filename)
      (message "Gtags updated for %s" filename))

    (defun gtags-update-hook()
      "Update GTAGS file incrementally upon saving a file"
      (when gtags-mode
        (when (gtags-root-dir)
          (gtags-update-current-file))))

    (add-hook 'after-save-hook 'gtags-update-hook)
```

## Database Content

To list the content in the generated files, e.g., GTAGS, execute the following command.

```
gtags -d GTAGS | less
```

The pipe is obivously not required.



## Emacs

Helm[^3] is expected to be installed. It's just so much easier to have
everything nicely with fuzzy-matching.

### Autocomplete

Autocomplete provides a pop-up menu instead of blindly cycling through
*vhdl-mode*'s recommendations.

```lisp
;;--------------------------------------------------------------------------------
;;    emacs VHDL autocomplete menu
;;--------------------------------------------------------------------------------
(require 'auto-complete)
(add-hook 'vhdl-mode-hook 'auto-complete-mode)
```

### Helm-gtags

Now your favourite *helm-gtags-dwim* should jump wherever your heart
desires to be.

**TODO** helm-config

# Further reading

http://csantosb.github.io/vhdl-tools/


# Python

Because there's lots of snaky stuff out there.

## Installation

```shell
pip install ropemacs
cd ~/.dotfiles/emacs/Pymacs
make
make install
cd ~/.dotfiles/emacs/ropemacs
python setup.py install
```



[^1]: https://stackoverflow.com/a/15169556
[^2]: https://www.emacswiki.org/emacs/GnuGlobal#toc4
[^3]: https://github.com/emacs-helm/helm
