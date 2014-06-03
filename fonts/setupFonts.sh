#!/usr/bin/bash

mkdir -f ~/.fonts
cp ~/homeConfig/fonts/*.ttf ~/.fonts
fc-cache ~/.fonts
