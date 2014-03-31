# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

export LC_ALL=en_US.UTF-8  
export LANG=en_US.UTF-8

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

export PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:/Users/nurv/bolsa/utils/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/local/go/bin:/usr/local/MacGPG2/bin:/Users/nurv/bin/:/opt/local/bin:/Users/nurv/.gem/ruby/1.8/bin::/usr/local/mysql-5.5.21-osx10.6-x86_64/bin/"

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nano'
else
	if [[ $TERM == "xterm" || $TERM == "xterm-color" || $TERM == "rxvt" ]]; then
		alias EDITOR='nano'
	else
		alias EDITOR='nano'
	fi
fi

export ARCHFLAGS="-arch x86_64"
export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/usr/local/mysql/lib/
export JAVA_OPTS="-server -Xms256m -Xmx1024m -XX:PermSize=384m"
export GPGKEY=B621B2CE
export MAVEN_OPTS="$JAVA_OPTS -Dorg.apache.jasper.compiler.Parser.STRICT_QUOTE_ESCAPING=false"
alias vlc='open -a VLC'

#python manage.py anywhere

_pmanage_backsearch(){
	if [ ! -f manage.py ]; then
		if [[ `pwd` == "/" ]]; then
			echo "Not within a Django Project"
		else
			cd ..
			_pmanage_backsearch
		fi
	else
		if [[ $dir != `pwd` ]]; then
			echo "$fg[green]Using `pwd`$reset_color"
		fi
		python manage.py $args
	fi
}

function _pmanage(){
	local dir=`pwd`
	local args=$@
	_pmanage_backsearch
	cd $dir
}

alias p=_pmanage

#mvn anywhere

_mvn_backsearch(){
	if [ ! -f pom.xml ]; then
		if [[ `pwd` == "/" ]]; then
			echo "Not within a Maven Project"
		else
			cd ..
			_mvn_backsearch
		fi
	else
		if [[ $dir != `pwd` ]]; then
			echo "$fg[green]Using `pwd`$reset_color"
		fi
		mvn $args
	fi
}

function _mvn(){
	local dir=`pwd`
	local args=$@
	_mvn_backsearch
	cd $dir
}

alias mvn=_mvn

export LSCOLORS="exfxcxdxbxegedabagacad"
export LS_COLORS='di=34;40:ln=35;40:so=32;40:pi=33;40:ex=31;40:bd=34;46:cd=34;43:su=0;41:sg=0;46:tw=0;42:ow=0;43:'
export GREP_COLOR='1;33'
