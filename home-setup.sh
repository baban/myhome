#!/bin/bash

# gitからリポジトリを取り出し
git checkout-index -a -f --prefix=~/


# fzfコマンドをinstall
# https://qiita.com/tdrk/items/1eb27555ad65d63a6a25
git clone https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
