# Bash per login script
# this is for env vars and path 

if [[ -t "$fd" || -p /dev/stdin || -n "$PS1" ]]
then 
  . .bashrc
fi

#check for nicename, the CUL-IT server name utility
type nicename >/dev/null 2>&1
if [  $? -ne 0 ]; then
        CIT_SERVER="no"
    else	
        CIT_SERVER="yes"
fi

export PATH=$PATH:$HOME/bin

if [ "$CIT_SERVER" == "no" ]; then
    export ALTERNATE_EDITOR=emacs EDITOR=emacsclient VISUAL=emacsclient
else
    export ALTERNATE_EDITOR=vi EDITOR=vi VISUAL=vi
fi

if [ -e /opt_arxiv/perl ]; then
  export PATH=/opt_arxiv/perl/bin:$PATH
fi

#PDSH module to use by defulat
PDSH_RCMD_TYPE=ssh
