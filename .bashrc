# .bashrc

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

