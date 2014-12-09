# ~/.profile: executed by the command interpreter for login shells.
# this is useful for environment variables.
# Put scripts in .bashrc.sh
#
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.

# stuff like this is just for xterm to protect scp odd use of the shell 
if [ "$TERM" == 'xterm' ]; then
    echo "Who is on this server?" ; who ; echo
fi

# run the .bashrc even on login
if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
