PLATFORM='unknown'
unamestr=`uname`

if [[ "$unamestr" == 'Linux' ]]; then
   PLATFORM='linux'
elif [[ "$unamestr" == 'Darwin' ]]; then
   PLATFORM='darwin'
fi

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
    python manage.py "$@"
  fi
}

function _pmanage(){
  local dir=`pwd`
  _pmanage_backsearch $@
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
    mvn "$@"
  fi
}

function _mvn(){
  local dir=`pwd`
  _mvn_backsearch $@
  cd $dir
}

alias mvn=_mvn

#update dotenv

upenv(){
  pushd "$HOME/.env" > /dev/null
  git pull origin
  if [[ $SHELL == *bash* ]]; then
    source "$HOME/.bashrc"
  else
    source "$HOME/.zshrc"
  fi
  popd > /dev/null
}

function tabname {
  printf "\e]1;$1\a"
}

function winname {
  printf "\e]2;$1\a"
}

alias vlc='open -a VLC'

source "$HOME/.env/vars.sh"

