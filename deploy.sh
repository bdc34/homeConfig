#!/bin/bash

# Make links from home configuration git repository files
# to the places the are expected in the home directory.

ln -f -s $HOME/homeConfig/dot.emacs.el $HOME/.emacs
ln -f -s $HOME/homeConfig/basic.el $HOME/.basic.el
ln -f -s $HOME/homeConfig/dot.emacs-site-lisp $HOME/.emacs-site-lisp
ln -f -s $HOME/homeConfig/bash_profile $HOME/.bash_profile

mkdir -p $HOME/bin
for FILE in $( ls $HOME/homeConfig/bin )
do
 ln -f -s $HOME/homeConfig/bin/$FILE $HOME/bin
done
