# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

autoload colors; colors;

local HISTSIZE=100000
local SAVEHIST=100000
setopt hist_ignore_dups
setopt share_history
setopt hist_verify
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_space

plugins=(textmate python git brew  git-extras git-flow mvn osx pip django sublime terminalapp textmate)

source $ZSH/oh-my-zsh.sh


ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX="%f)"
ZSH_THEME_GIT_PROMPT_CLEAN="%B%F{green}✓%f%b"
ZSH_THEME_GIT_PROMPT_AHEAD="%F{cyan}▴%f"
ZSH_THEME_GIT_PROMPT_BEHIND="%F{magenta}▾%f"
ZSH_THEME_GIT_PROMPT_STAGED="%B%F{green}●%f%b"
ZSH_THEME_GIT_PROMPT_UNSTAGED="%B%F{yellow}●%f%b"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%B%F{red}●%f%b"

bureau_git_branch () {
  ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(command git rev-parse --short HEAD 2> /dev/null) || return
  echo "${ref#refs/heads/}"
}

get_space () {
  local STR=$1$2
  local zero='%([BSUbfksu]|([FB]|){*})'
  local LENGTH=${#${(S%%)STR//$~zero/}} 
  local SPACES=""
  (( LENGTH = ${COLUMNS} - $LENGTH - 1))

  echo ${COLUMNS}
}

bureau_git_status () {
  _INDEX=$(command git status --porcelain -b 2> /dev/null)
  _STATUS=""
  if $(echo "$_INDEX" | grep '^[AMRD]. ' &> /dev/null); then
    _STATUS="$_STATUS$ZSH_THEME_GIT_PROMPT_STAGED"
  fi
  if $(echo "$_INDEX" | grep '^.[MTD] ' &> /dev/null); then
    _STATUS="$_STATUS$ZSH_THEME_GIT_PROMPT_UNSTAGED"
  fi
  if $(echo "$_INDEX" | grep -E '^\?\? ' &> /dev/null); then
    _STATUS="$_STATUS$ZSH_THEME_GIT_PROMPT_UNTRACKED"
  fi
  if $(echo "$_INDEX" | grep '^UU ' &> /dev/null); then
    _STATUS="$_STATUS$ZSH_THEME_GIT_PROMPT_UNMERGED"
  fi
  if $(command git rev-parse --verify refs/stash >/dev/null 2>&1); then
    _STATUS="$_STATUS$ZSH_THEME_GIT_PROMPT_STASHED"
  fi
  if $(echo "$_INDEX" | grep '^## .*ahead' &> /dev/null); then
    _STATUS="$_STATUS$ZSH_THEME_GIT_PROMPT_AHEAD"
  fi
  if $(echo "$_INDEX" | grep '^## .*behind' &> /dev/null); then
    _STATUS="$_STATUS$ZSH_THEME_GIT_PROMPT_BEHIND"
  fi
  if $(echo "$_INDEX" | grep '^## .*diverged' &> /dev/null); then
    _STATUS="$_STATUS$ZSH_THEME_GIT_PROMPT_DIVERGED"
  fi
  _STATUS="$_STATUS"
  echo $_STATUS
}

user_host(){
  return "%B%F{green}%n@%m%f%b"
}

bureau_git_prompt () {
  local _branch=$(bureau_git_branch)
  local _status="$(bureau_git_status)"
  if [[ "${_branch}x" != "x" ]]; then
    _result="$ZSH_THEME_GIT_PROMPT_PREFIX$_branch"
    if [[ "${_status}x" != "x" ]]; then
      _result="$_result $_status"
    fi
    _result="$_result$ZSH_THEME_GIT_PROMPT_SUFFIX"
  fi
    if [ "${#_result}" -ne "0" ]; then
    _result=" $_result"
  fi
  echo $_result
}

local host_path="%B%F{green}%m%f%b"
current_dir(){
  echo "%B%F{blue}${PWD/#$HOME/~}%f%b"
}

get_user(){
  if [[ $LOGNAME != $USER ]]; then
    echo "%n"
  fi
}

get_user(){
  local result="";
  if [[ $LOGNAME != "nurv" ]]; then
    result="$result%USERNAME"
  fi

  if [[ $LOGNAME != "nurv" && -n $SSH_CONNECTION ]]; then
    result="$result@"
  fi

  if [[ -n $SSH_CONNECTION ]]; then
    result="$result$(command hostname -s) %B%F{cyan}★%f%b"
  fi

  if [ "${#result}" -ne "0" ]; then
    result="$result "
  else
    result=""
  fi
  echo "$result"
}

get_host(){
  echo "%B%F{green}$(get_user)%f%b"
}

_get_virtualenv(){
  if test -z "$VIRTUAL_ENV" ; then
      echo ""
  else
      echo " %F{yellow}⬡ `basename \"$VIRTUAL_ENV\"`%f"
  fi
}

_get_pom(){
  POM=$(command perl "$HOME/.env/getpom.pl")
  if [ -z "$POM" ]; then
      echo ""
  else
      echo " %F{yellow}⬡ `basename \"$POM\"`%f"
  fi
}

function set_prompt_symbol () {
  if [ $RETURN_STATUS -eq 0 ]; then
    echo "[!]"
  else
    echo "[%F{cyan}!%f}]: "
  fi
}

has_jobs(){
  local j="$(jobs -p)"
  if [[ -n $j ]]; then
    echo " %F{magenta}♨%f}"
  else
    echo ""
  fi
}

function __prompt_command(){
  RETURN_STATUS=$?
  local hud="$(get_host)$(current_dir)$(bureau_git_prompt)$(_get_virtualenv)$(_get_pom)"
  if [ "${#hud}" -gt $(($COLUMNS + 15*5)) ]; then
    PS1='╭ $(get_host)$(current_dir)$(bureau_git_prompt)$(_get_virtualenv)$(_get_pom)
╰$(set_prompt_symbol) '
  else
    PS1='[$(get_host)$(current_dir)$(has_jobs)$(bureau_git_prompt)$(_get_virtualenv)$(_get_pom)]: '
  fi
}

export VIRTUAL_ENV_DISABLE_PROMPT=1
export PROMPT_COMMAND=__prompt_command

precmd(){
  __prompt_command
}


source "$HOME/.env/common.sh"
