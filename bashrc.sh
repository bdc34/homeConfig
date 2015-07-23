# .bashrc 
# This is run for interactive shells and for logins by .profile

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
umask 002
export RAN_BASHRC=`date`;

# # Change pdsh ssh args to forward ssh agent for doing git pulls
# export PDSH_SSH_ARGS="-2 -A -x -l%u %h" 
# # PDSH module to use by defulat
# export PDSH_RCMD_TYPE="ssh"

# # server groups for use with pdsh -w $wxyz
# export warxivprod="arxiv-export,arxiv-export[1-2],arxiv-web[1-3],arxiv-db,arxiv-db[2-3],arxiv-nexus,arxiv-res"
# export warxivdev="arxiv-dev,arxiv-beta1"
# export warxivall="$warxivdev,$warxivprod"
# export wcularprod="cular,cular-follower"
# export wcularall="$wcularprod,cular-dev"
# export wcornellall="bdc34-dev,$wcularall,$warxivall"

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend
PROMPT_COMMAND='history -a'

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export history=1000
export savehist=40

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
    xterm) color_prompt=yes;;
esac


# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi


type nicename >/dev/null 2>&1
if [  $? -ne 0 ]; then
        PROMPT_H='\h'
        CIT_SERVER="no"
    else	
        PROMPT_H=$(nicename)
        CIT_SERVER="yes"
fi

if [ "$color_prompt" = yes ]; then
    PS1="${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@$PROMPT_H\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ "
else
    PS1="${debian_chroot:+($debian_chroot)}\u@$PROMPT_H:\W\$ "
fi
unset color_prompt force_color_prompt

if [ -e /opt_arxiv/perl ]; then
  export PATH=/opt_arxiv/perl/bin:$PATH
fi

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@$PROMPT_H: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    #alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Change pdsh ssh args to forward ssh agent for doing git pulls
export PDSH_SSH_ARGS="-2 -A -x -l%u %h" 
# PDSH module to use by defulat
export PDSH_RCMD_TYPE="ssh"
# Server groups for use with PDSH -w
export warxivprod="arxiv-export,arxiv-export[1-2],arxiv-web[1-3],arxiv-db,arxiv-db[2-3],arxiv-nexus,arxiv-res"
export warxivdev="arxiv-dev,arxiv-beta1"
export warxivall="$warxivdev,$warxivprod"
export wcularprod="cular,cular-follower"
export wcularall="$wcularprod,cular-dev,cular-ingest"
export wcornellall="bdc34-dev,$wcularall,$warxivall"

# Simeon W:
# If this is an arXiv machine then add some extra stuff

if [ "$CIT_SERVER" == 'yes' ] && [[ `nicename` =~ 'arxiv' ]] ; then
    alias fd='/users/e-prints/bin/dev/finddef.pl'
    alias fu='/users/e-prints/bin/dev/finduse.pl'
    alias ep='sudo su - e-prints'
    alias rl='sudo /users/e-prints/bin/rl'
    alias cpan='sudo sh -c "/opt_arxiv/perl/bin/perl -MCPAN -e shell"'
    alias mysql_login.pl=/users/e-prints/bin/mysql_login.pl
    SYSTEM_TYPE='arxiv'
else
    alias fd='echo "This is not an arxiv machine."'
    alias fu='echo "This is not an arxiv machine."'
    alias ep='echo "This is not an arxiv machine."'
    alias rl='echo "This is not an arxiv machine."'
    alias mysql_login.pl='echo "This is not an arxiv machine."'
    SYSTEM_TYPE='cit'
fi


# display grants for a whole mysql database
mygrants()
{
  mysql -B -N $@ -e "SELECT DISTINCT CONCAT(
    'SHOW GRANTS FOR ''', user, '''@''', host, ''';'
    ) AS query FROM mysql.user" | \
  mysql $@ | \
  sed 's/\(GRANT .*\)/\1;/;s/^\(Grants for .*\)/## \1 ##/;/##/{x;p;x;}'
}

if [ -e ~/.bash-local ]; then
  source ~/.bash-local
fi

