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

# change pdsh ssh args to
# forward ssh agent for doing git pulls
export PDSH_SSH_ARGS="-2 -A -x -l%u %h" 
#PDSH module to use by defulat
export PDSH_RCMD_TYPE="ssh"

#server groups

export warxivprod="arxiv-export,arxiv-export[1-2],arxiv-web[1-3],arxiv-db,arxiv-db[2-3],arxiv-nexus,arxiv-res"
export warxivdev="arxiv-dev,arxiv-beta1"
export warxivall="$warxivdev,$warxivprod"
export wcularprod="cular,cular-follower"
export wcularall="$wcularprod,cular-dev"
export wcornellall="bdc34-dev,$wcularall,$warxivall"
