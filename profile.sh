# ~/.profile: executed by the command interpreter for login shells.
# this is useful for environment variables.
# Put scripts in .bashrc.sh
#
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.


export RAN_PROFILE=`date`;

if [ -e /users/bdc34/bin ]; then
    export PATH=$PATH:/users/bdc34/bin
fi
if [ -e $HOME/bin ]; then
    export PATH=$PATH:$HOME/bin
fi


# stuff like this is just for xterm to protect scp odd use of the  shell 
if [ "$TERM" == 'xterm' ]; then
    echo "Who is on this server?" ; who ; echo
    if [ -e /usr/local/bin/nicename ]; then
        echo "This is an CUL-CIT machine. Nicename: " `nicename` "==" `hostname`
    fi
fi

# run the .bashrc even on login
if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
