# .bashrc

export PS1="\[\033[0m\][\u@\h:\W]$ "

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions

if [ -f ~/.sh_aliases ]; then
    . ~/.sh_aliases
fi

HISTSIZE=10000
HISTFILESIZE=10000
HISTTIMEFORMAT='%y/%m/%d %H:%M:%S  '   #←日付、時間
HISTTIMEFORMAT='%H:%M:%S '             #←時間
HISTIGNORE=ls:history                  #←historyに記録しないコマンド

# バックグラウンドジョブの終了を通知する
set -o notify
# 標準出力からのファイルの上書きを忠告する
set -o noclobber
# Ctrl+D でログアウトしない
set -o ignoreeof
# 未定義の変数を参照するとエラー・メッセージを表示する
set -o nounset
#set -o xtrace          # Useful for debuging.
# 未定義の変数を参照するとエラー・メッセージを表示する
set -o emacs
# シェルのブレース展開を有効にする。
set -o braceexpand 
# コマンドの位置を記憶し，検索時間を短縮する
set -o hashall

