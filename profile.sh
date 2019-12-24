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
if [ -e /opt_arxiv/perl ]; then
  export PATH=/opt_arxiv/perl/bin:$PATH
fi
if [ -e /opt/node ]; then
  export PATH=/opt/node/bin:$PATH
fi


type pdsh >/dev/null 2>&1
if [ $? -ne 0 ]; then
    # Change pdsh ssh args to forward ssh agent for doing git pulls
    export PDSH_SSH_ARGS="-2 -A -x -l%u %h" 
    # PDSH module to use by defulat
    export PDSH_RCMD_TYPE="ssh"

    # server groups for use with pdsh -w $wxyz
    export warxivprod="arxiv-export[1-3],arxiv-web[1-4],arxiv-db,arxiv-db[2-3],arxiv-nexus"
    export warxivdev="arxiv-dev,arxiv-beta1"
    export warxivall="$warxivdev,$warxivprod"
    export wcornellall="bdc34-dev,$warxivall"
fi


# stuff like this is just for xterm to protect scp odd use of the shell 
if [ "$TERM" == 'xterm' ]; then
    if [ -e /usr/local/bin/nicename ]; then
        echo "Who is on this server?" ; who ; echo
        echo "This is an CUL-CIT machine. Nicename: " `nicename` "==" `hostname`
    fi
fi

if [ -d /home/bdc34/.pyenv ]; then
    export PATH="/home/bdc34/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi
