#!/usr/bin/bash


# Some font info: https://pandasauce.org/post/linux-fonts/

if [ ! -e ~/.fonts ] 
then
	mkdir ~/.fonts
fi
cp ~/homeConfig/fonts/*.ttf ~/.fonts
cp ~/homeConfig/fonts/fonts.conf ~/.fonts.conf
unzip ~/homeConfig/fonts/*.zip -d ~/.fonts/
fc-cache ~/.fonts

sudo apt install fonts-noto  ttf-mscorefonts-installer
