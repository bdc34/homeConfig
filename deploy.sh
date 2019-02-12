#!/bin/bash

# Make links from home configuration git repository files
# to the places the are expected in the home directory.

ln -f -s $HOME/homeConfig/dot.emacs.el $HOME/.emacs
ln -f -s $HOME/homeConfig/basic.el $HOME/.basic.el
if [ ! -e $HOME/.emacs-site-lisp ]
   then
       ln -f -s $HOME/homeConfig/dot.emacs-site-lisp $HOME/.emacs-site-lisp
fi

if [ ! -e  $HOME/.emacs-site-lisp/use-package ]
   then
       ln -f -s $HOME/homeConfig/use-package $HOME/.emacs-site-lisp/
fi

mkdir -p $HOME/.emacs.d
ln -f -s $HOME/bin/psh.pl $HOME/.emacs.d/psh.pl

ln -f -s $HOME/homeConfig/profile.sh $HOME/.profile
ln -f -s $HOME/homeConfig/bashrc.sh $HOME/.bashrc


mkdir -p $HOME/bin
for FILE in $( ls $HOME/homeConfig/bin )
do
 ln -f -s $HOME/homeConfig/bin/$FILE $HOME/bin
done

mkdir -p $HOME/.ssh
chmod 700 $HOME/.ssh
chmod 700 $HOME/homeConfig/ssh_config
ln -f -s $HOME/homeConfig/ssh_config $HOME/.ssh/config
