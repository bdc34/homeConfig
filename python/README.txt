How I'm working with pipenv and emacs elpy

1. Install pyenv so versions of python can be installed. (not using it for virtenv or anything)
2. down load a arxiv repo from git.
3. run pipenv install
4. run pipenv shell
5. install all the elpy required modules:
pip install autopep8 flake8 importmagic jedi pyflakes yapf mypy pylint
pip install ipykernel
python -m ipykernel install --user

2022-02-24 Using lsp-pyrite instead
