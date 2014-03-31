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

source "$HOME/.env/common.sh"