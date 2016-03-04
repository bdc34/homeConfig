#!/usr/bin/bash

if [ ! -e ~/.fonts ] 
then
	mkdir ~/.fonts
fi
cp ~/homeConfig/fonts/*.ttf ~/.fonts
cp ~/homeConfig/fonts/fonts.conf ~/.fonts.conf
unzip ~/homeConfig/fonts/*.zip -d ~/.fonts/
fc-cache ~/.fonts
