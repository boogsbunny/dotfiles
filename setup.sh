#!/bin/bash

# make backup directory
mkdir -p ~/dotfiles/backups

# create symbolic links
echo "[+] Creating symbolic links, moving current dotfiles to ~/dotfiles/backups"
for dotfile in .vimrc .zshrc .zpreztorc .zshenv .aliases .tmux.conf; do
	if [ -f ~/$dotfile ]; then
		mv --backup=t ~/$dotfile ~/dotfiles/backups
	fi
	ln -s ~/dotfiles/$dotfile ~/$dotfile
	echo "[+] Created $dotfile"
done

# hyperterm for mac

# create i3 setup
echo "Would you like to set up i3 for linux?"
read answer
if [ "$answer" != "${answer#[Yy]}" ] ;then

	for i3config in ~/dotfiles/.config/i3/config ~/dotfiles/.config/i3/i3blocks.conf; do
		if [ -f ~/.config/i3/$i3config ]; then
			mv --backup=t ~/.config/i3/$i3config ~/dotfiles/backups
		fi
		ln -s ~/dotfiles/.config/i3/$i3config ~/.config/i3/$i3config
		echo "[+] Created $i3config"
	done

	for polybar in ~/dotfiles/.config/polybar/launch.sh ; do
		if [ -f ~/.config/polybar/$polybar ]; then
			mv --backup=t ~/.config/polybar/$polybar ~/dotfiles/backups
		fi
		ln -s ~/dotfiles/.config/polybar/$polybar ~/.config/polybar/$polybar
		echo "[+] Created $polybar"
	done

	# for font in fontawesome-webfont.ttf SystemSanFranciscoDisplayBold.ttf SystemSanFranciscoDisplayRegular.ttf SystemSanFranciscoDisplayThin.ttf SystemSanFranciscoDisplayUltralight.ttf ; do
	# 	if [ -f ~/.fonts/$font ]; then
	# 		mv --backup=t ~/.fonts/$font ~/dotfiles/backups
	# 	fi
	# 	ln -s ~/dotfiles/.fonts/$font ~/.fonts/$font
	# 	echo "[+] Created $font"
	# done
else
    echo Enjoy your lame setup without it
fi

# Arch linux

# install plugged
if [ ! -d ~/.vim/plugged ]; then
	 curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		 https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# install vim plugins
vim +PlugInstall +qall

# make vim undodir
mkdir -p ~/.vim/undodir

# link ycm config
# if [ -f ~/.vim/.ycm_extra_conf.py ]; then
# 	mv --backup=t ~/.vim/.ycm_extra_conf.py ~/dotfiles/backups
# fi
# ln -s ~/dotfiles/.ycm_extra_conf.py ~/.vim

# install prezto
if  [ ! -d ~/.zprezto ]; then
	echo "[+] Installing prezto"
	if [ -x "$(command -v git)" ]; then
		git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
	else
		echo "[-] Git is not installed, install with sudo pacman -S git"
	fi
fi

# copy over prezto dotfiles
for dotfile in zlogin zlogout zprofile; do
	if [ -f ~/.$dotfile ]; then
		mv --backup=t ~/.$dotfile ~/dotfiles/backups
	fi
	ln -s ~/.zprezto/runcoms/$dotfile ~/.$dotfile
done

# install pyenv
if  [ ! -d ~/.pyenv ]; then
	echo "[+] Installing pyenv"
	if [ -x "$(command -v git)" ]; then
		git clone https://github.com/pyenv/pyenv.git ~/.pyenv
	else
		echo "[-] Git is not installed, install with sudo pacman -S git"
	fi
fi

echo "[+] dotfiles installed. Your old dotfiles have been placed in ~/dotfiles/backups"
echo "run yaourt -S silver-searcher-git universal-ctags-git to install dependencies for vim plugins"
