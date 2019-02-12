sudo apt-get install -y git python-pip python3-pip  \
     make build-essential \
     libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev

# What is the best way to install Python on a Unix?

# For me pyenv is the best way to install Python on a Mac or
# Linux. Everything gets installed under your home directoy, without
# tampering with the rest of the system. Besides that, it supports
# many different Python implementation such as PyPy, Anaconda,
# CPython, etc. All with one command.

# First we need to install pyenv and two of its extensions,
# pyenv-virtualenv and pyenv-virtualenvwrapper. I use each one for
# different purposes:

# I use pyenv to install Python interpreters;
# I use pyenv-virtualenv to configure my “global environment”;
# I use pyenv-virtualenvwrapper to work on my projects;

if [-e /usr/bin/apt-get ]; then
    sudo apt-get install -y make build-essential libssl-dev zlib1g-dev libbz2-dev \
         libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev \
         xz-utils tk-dev
fi

if [-e /usr/bin/yum ]; then
    sudo yum install zlib-devel bzip2 bzip2-devel readline-devel sqlite sqlite-devel \
         openssl-devel xz xz-devel openssl-devel \
         python-devel python34-devel
fi

git clone https://github.com/pyenv/pyenv.git ~/.pyenv
#git clone https://github.com/pyenv/pyenv-virtualenv.git $(~/.pyenv/bin/pyenv root)/plugins/pyenv-virtualenv
#git clone https://github.com/pyenv/pyenv-virtualenvwrapper.git $(~/.pyenv/bin/pyenv root)/plugins/pyenv-virtualenvwrapper

export PATH=~/.pyenv/bin:$PATH
export WORKON_HOME=~/.ve
export PROJECT_HOME=~/workspace
eval "$(pyenv init -)"

pyenv install 3.6.3
pyenv install 2.7.13

# Create some envs
pyenv virtualenv 2.7.13 tools2
pyenv activate tools2
pip install rename s3cmd fabric
pyenv deactivate

pyenv virtualenv 3.6.3 dev3
pip install autopep8 flake8 importmagic jedi pyflakes yapf mypy pylint
pip install ipykernel
python -m ipykernel install --user
pyenv deactivate

pyenv global 3.6.5 2.7.13 dev3 tools2
#The above command establishes the PATH priority so scripts can be
#accessed in the right order without activating any virtualenv.

