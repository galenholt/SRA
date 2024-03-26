REM THIS SHOULD BE DONE ONCE PER PROJECT
REM Expects python, pyenv, and poetry exist. Use project_setup.sh on linux

REM set up the python
REM assume the user has python, pyenv, and poetry
call pyenv local 3.11.0

REM now install packages from poetry.lock
call poetry install

REM set up R
call Rscript "R_setup.R"

echo Finished R setup
@pause
