#
# .zshrc is sourced in interactive shells.
# It should contain commands to set up aliases,
# functions, options, key bindings, etc.
#

autoload -U colors; colors
#PROMPT=$'\n%{\e[1;32m%}%n@%m %{\e[1;33m%}%~\n%{\e[1;m%}%(!.#.$) '
#SPROMPT="correct: %R -> %r ? "

PROMPT=$'%n@%m %~ %% '
SPROMPT="correct: %R -> %r ? "


autoload -U compinit
compinit

#allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

## keep background processes at full speed
#setopt NOBGNICE
## restart running processes on exit
#setopt HUP

## history
#setopt APPEND_HISTORY
## for sharing history between zsh processes
#setopt INC_APPEND_HISTORY
#setopt SHARE_HISTORY

## never ever beep ever
#setopt NO_BEEP

## automatically decide when to page a list of completions
#LISTMAX=0

## disable mail checking
#MAILCHECK=0

# autoload -U colors
#colors

# 関数
find-grep () { find . -type f -print | xargs grep -n --binary-files=without-match $@ }

# エイリアスの設定
alias ll='ls -ltr'
alias gd='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000


# 履歴ファイルに時刻を記録
setopt extended_history

# 補完するかの質問は画面を超える時にのみに行う｡
LISTMAX=0

autoload -Uz compinit; compinit

# sudo でも補完の対象
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

# cdのタイミングで自動的にpushd
setopt auto_pushd 

# 複数の zsh を同時に使う時など history ファイルに上書きせず追加
setopt append_history

# 補完候補が複数ある時に、一覧表示
setopt auto_list

# 保管結果をできるだけ詰める
setopt list_packed
# 補完キー（Tab, Ctrl+I) を連打するだけで順に補完候補を自動で補完
setopt auto_menu

# カッコの対応などを自動的に補完
setopt auto_param_keys

# ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash

# ビープ音を鳴らさないようにする
setopt no_beep

# 直前と同じコマンドラインはヒストリに追加しない
setopt hist_ignore_dups

# 余分なスペースを削除してヒストリに記録する
setopt hist_reduce_blanks

# 行頭がスペースで始まるコマンドラインはヒストリに記録しない
# setopt hist_ignore_spece

# 重複したヒストリは追加しない
# setopt hist_ignore_all_dups

# ヒストリを呼び出してから実行する間に一旦編集できる状態になる
setopt hist_verify

# auto_list の補完候補一覧で、ls -F のようにファイルの種別をマーク表示しない
setopt no_list_types

# コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
setopt magic_equal_subst

# ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt mark_dirs

# 8 ビット目を通すようになり、日本語のファイル名を表示可能
setopt print_eight_bit

# シェルのプロセスごとに履歴を共有
setopt share_history

# Ctrl+wで､直前の/までを削除する｡
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# ディレクトリを水色にする｡
export LS_COLORS='di=01;36'

# ファイルリスト補完でもlsと同様に色をつける｡
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# cd をしたときにlsを実行する
function chpwd() { ls }

# ディレクトリ名だけで､ディレクトリの移動をする｡
setopt auto_cd

# C-s, C-qを無効にする。
setopt no_flow_control


# git のエイリアス
# http://tobysoft.net/wiki/index.php?git%2F%A5%B3%A5%DE%A5%F3%A5%C9%A4%CE%BE%CA%CE%AC%28alias%29%C0%DF%C4%EA%A4%F2%A4%B9%A4%EB%CA%FD%CB%A1


# 環境ごとの独自設定を記述
case `uname` in 
  "Linux")   [ -f ~/.zshrc.linux  ] && source ~/.zshrc.linux;;
  "Darwin")  [ -f ~/.zshrc.mac    ] && source ~/.zshrc.mac;;
esac


alias gco="git checkout"
alias gst="git status"
alias gci="git commit -a"
alias gdi="git diff"
alias gbr="git branch"

function git-init(){
	git config --global alias.co checkout
	git config --global alias.c checkout
	git config --global alias.st 'status'
	git config --global alias.s 'status'
	git config --global alias.ci 'commit -a '
	git config --global alias.di 'diff'
	git config --global alias.br 'branch'
	git config --global alias.b 'branch'
	git config --global alias.log 'log --name-status'
	git config --global alias.l 'log --name-status'
}



function date-cp(){ cp $1 "$1"_`date +"%Y%m%d"` }
alias hres="sudo /etc/init.d/httpd restart"
alias his="history 0"

[ -f ~/.zshrc.mine ] && source ~/.zshrc.mine
