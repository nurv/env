pushd $HOME > /dev/null

platform='unknown'
unamestr=`uname`

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

# wiring
ln -s "$HOME/.env/zshrc" "$HOME/.zshrc"
ln -s "$HOME/.env/bashrc" "$HOME/.bashrc"
ln -s "$HOME/.env/bashrc" "$HOME/.bash_profile"

popd > /dev/null