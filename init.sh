die() { echo "$*" 1>&2 ; exit 1; }
git --version 2>&1 >/dev/null
GIT_IS_AVAILABLE=$?

if [ $GIT_IS_AVAILABLE -ne 0 ]; then
    die "Git not installed"
fi

zsh --version 2>&1 >/dev/null
ZSH_IS_AVAILABLE=$?

if [ $ZSH_IS_AVAILABLE -ne 0 ]; then
    die "ZSH not installed"
fi

pushd $HOME > /dev/null

platform='unknown'
unamestr=`uname`

if [[ ! -d "$HOME/.env" ]]; then
    if [[ "$unamestr" == 'Linux' ]]; then
       platform='linux'
    elif [[ "$unamestr" == 'Darwin' ]]; then
       platform='darwin'
    fi

    if [ ! -n "$ZSH" ]; then
      ZSH=~/.oh-my-zsh
    fi

    echo "Cloning env folder"
    git clone https://github.com/nurv/env.git .env > /dev/null
    
    if [ ! -d "$ZSH" ]; then
      echo "Cloning oh my zsh"
      git clone https://github.com/robbyrussell/oh-my-zsh.git $ZSH > /dev/null
    fi

    echo "Cloning plugins"
    git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
    git clone https://github.com/zsh-users/zsh-history-substring-search ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-history-substring-search

    echo "Rewiring..."

    # backingup old stuff
    zshconfig="$HOME/.zshrc"

    if [ -f $zshconfig ]; then
    	mv $zshconfig "$zshconfig.old"
    fi

    if [ -f "$HOME/.bash_profile" ]; then
    	mv "$HOME/.bash_profile" "$HOME/.bash_profile.old"
    fi

    if [ -f "$HOME/.bashrc" ]; then
      mv "$HOME/.bashrc" "$HOME/.bashrc.old"
    fi

    if [ -f "$HOME/.gitconfig" ]; then
      mv "$HOME/.gitconfig" "$HOME/.gitconfig.old"
    fi

    if [ -f "$HOME/.screenrc" ]; then
      mv "$HOME/.screenrc" "$HOME/.screenrc.old"
    fi

    if [ -f "$HOME/.emacs" ]; then
      mv "$HOME/.emacs" "$HOME/.emacs.old"
    fi

    if [ -d "$HOME/.emacs.d" ]; then
      mv "$HOME/.emacs.d" "$HOME/.emacs.d.old"
    fi

    # wiring
    ln -s "$HOME/.env/zshrc" "$HOME/.zshrc"
    ln -s "$HOME/.env/bashrc" "$HOME/.bashrc"
    ln -s "$HOME/.env/bashrc" "$HOME/.bash_profile"
    ln -s "$HOME/.env/gitconfig" "$HOME/.gitconfig"
    ln -s "$HOME/.env/screenrc" "$HOME/.screenrc"
    ln -s "$HOME/.env/emacs" "$HOME/.emacs"
    ln -s "$HOME/.env/emacs.d" "$HOME/.emacs.d"

else
    echo "You have a env. Trying to update"
    upenv
fi

popd > /dev/null
