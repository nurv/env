pushd $HOME

platform='unknown'
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
   platform='linux'
elif [[ "$unamestr" == 'Darwin' ]]; then
   platform='darwin'
fi

echo "Cloning env folder"
git clone https://github.com/nurv/env.git .env 

echo "Rewiring..."

# backingup old stuff
zshconfig="~/.zshrc"
if [[ $platform == "linux" ]]; then
	bashconfig="~/.bashrc"
else
	bashconfig="~/.bash_profile"	
fi

if [ -f $zshconfig ]; then
	cp $zshconfig "$zshconfig.old"
fi

if [ -f $bashconfig ]; then
	cp $bashconfig "$bashconfig.old"
fi

# wiring
ln -s "$HOME/.env/zshrc" "$HOME/.zshrc"
ln -s "$HOME/.env/bashrc" "$HOME/.bashrc"
ln -s "$HOME/.env/bashrc" "$HOME/.bash_profile"

popd 