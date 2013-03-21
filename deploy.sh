#!/bin/bash

# Make links from home configuration git repository files
# to the places the are expected in the home directory.

ln -f -s $HOME/homeConfig/dot.emacs.el $HOME/.emacs
ln -f -s $HOME/homeConfig/dot.emacs-site-lisp $HOME/.emacs-site-lisp
