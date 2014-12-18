# Bash per login script
# this is for env vars and path 

export RAN_BASH_PROFILE=`date`

if [[ -t "$fd" || -p /dev/stdin || -n "$PS1" ]]
then 
  . .bashrc
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

