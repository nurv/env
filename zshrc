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
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%})"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[green]%}✓%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_AHEAD="%{$fg[cyan]%}▴%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_BEHIND="%{$fg[magenta]%}▾%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_STAGED="%{$fg_bold[green]%}●%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_UNSTAGED="%{$fg_bold[yellow]%}●%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg_bold[red]%}●%{$reset_color%}"

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
  return "%{$terminfo[bold]$fg[green]%}%n@%m%{$reset_color%}"
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
  echo $_result
}

local host_path="%{$terminfo[bold]$fg[green]%}%m%{$reset_color%}"

local current_dir="%{$terminfo[bold]$fg[blue]%}%~%{$reset_color%}"

get_user(){
  if [[ $LOGNAME != $USER ]]; then
    echo "%n"
  fi
}

get_user(){
  local result="";
  if [[ $LOGNAME != "nurv" ]]; then
    result="$result%n"
  fi

  if [[ $LOGNAME != "nurv" && -n $SSH_CONNECTION ]]; then
    result="$result@"
  fi

  if [[ -n $SSH_CONNECTION ]]; then
    result="$result%m $terminfo[bold]$fg[cyan]★%{$reset_color%}"
  fi

  if [ "${#result}" -ne "0" ]; then
    result="$result "
  else
    result=""
  fi
  echo "$result"
}

get_host(){
  echo "%{$terminfo[bold]$fg[green]%}$(get_user)%{$reset_color%}"
}

_get_virtualenv(){
  if test -z "$VIRTUAL_ENV" ; then
      echo ""
  else
      echo "$fg[yellow]⬡ `basename \"$VIRTUAL_ENV\"`$reset_color"
  fi
}

_get_pom(){
  POM=$(command perl "$HOME/.env/getpom.pl")
  if [ -z "$POM" ]; then
      echo ""
  else
      echo "$fg[yellow]⬡ `basename \"$POM\"`$reset_color"
  fi
}

function set_prompt_symbol () {
  if [ $RETURN_STATUS -eq 0 ]; then
    echo "[!]"
  else
    echo "[$fg[cyan]!${reset_color}]"
  fi
}

function __prompt_command(){
  RETURN_STATUS=$?
  PS1='╭ $(get_host)${current_dir} $(bureau_git_prompt) $(_get_virtualenv)$(_get_pom)
╰$(set_prompt_symbol) '
}

export VIRTUAL_ENV_DISABLE_PROMPT=1
export PROMPT_COMMAND=__prompt_command

precmd(){
  __prompt_command
}


source "$HOME/.env/common.sh"
