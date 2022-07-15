autoload -U colors; colors
autoload -U compinit; compinit

PROMPT=$'[%n@%m %1~]%% '
SPROMPT="correct: %R -> %r ? "

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# Emacsライクキーバインド設定
bindkey -e

limit coredumpsize 102400

# sudo でも補完の対象
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

# 補完するかの質問は画面を超える時にのみに行う｡
LISTMAX=0

setopt auto_cd # ディレクトリ名だけで､ディレクトリの移動をする｡
setopt auto_menu # 補完キー（Tab, Ctrl+I) を連打するだけで順に補完候補を自動で補完
setopt auto_pushd  # cdのタイミングで自動的にpushd
setopt append_history # 複数の zsh を同時に使う時など history ファイルに上書きせず追加
setopt auto_list # 補完候補が複数ある時に、一覧表示
setopt auto_param_keys # カッコの対応などを自動的に補完
setopt auto_param_slash # ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt no_beep # ビープ音を鳴らさないようにする
setopt brace_ccl # {a-za-z} をブレース展開
setopt correct # スペルチェック
setopt complete_in_word  #allow tab completion in the middle of a word
setopt equals # =command を command のパス名に展開する
setopt extended_history # 履歴ファイルに時刻を記録
setopt no_flow_control # C-s, C-qを無効にする。
setopt hist_ignore_dups # 直前と同じコマンドラインはヒストリに追加しない
setopt hist_reduce_blanks # 余分なスペースを削除してヒストリに記録する
# setopt hist_ignore_spece # 行頭がスペースで始まるコマンドラインはヒストリに記録しない
# setopt hist_ignore_all_dups # 重複したヒストリは追加しない
setopt hist_verify # ヒストリを呼び出してから実行する間に一旦編集できる状態になる
setopt no_hup # 走行中のジョブにシグナルを送らない
setopt no_list_beep # 補完の時にベルを鳴らさない
setopt list_types # 補完候補一覧でファイルの種別をマーク表示
setopt list_packed # 保管結果をできるだけ詰める
setopt magic_equal_subst # コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
setopt mark_dirs # ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt print_eight_bit # 8 ビット目を通すようになり、日本語のファイル名を表示可能
setopt no_promptcr # 出力の文字列末尾に改行コードが無い場合でも表示
setopt prompt_subst # 色を使う
setopt pushd_ignore_dups # 同じディレクトリを pushd しない
setopt share_history # シェルのプロセスごとに履歴を共有


# Ctrl+wで､直前の/までを削除する｡
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# ディレクトリを水色にする｡
export LS_COLORS='di=01;36'

# ファイルリスト補完でもlsと同様に色をつける｡
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# コアダンプサイズを制限
# エイリアスの設定
alias ll='ls -ltr'
alias gd='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'

# cd をしたときにlsを実行する
function chpwd() { ls }

# bash zsh 共通の拡張設定
[ -f ~/.xshrc ] && source ~/.xshrc
[ -f ~/.xshrc.mime ] && source ~/.xshrc.mine

# 環境ごとの独自設定を記述
case `uname` in 
  "Linux")   [ -f ~/.zshrc.linux  ] && source ~/.zshrc.linux;;
  "Darwin")  [ -f ~/.zshrc.mac    ] && source ~/.zshrc.mac;;
esac

# ホストサーバーごとの依存の設定を追記
[ -f ~/.zshrc.mine ] && source ~/.zshrc.mine


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


alias be="bundle exec"
alias bec="bundle exec rails c"
alias ber="bundle exec rails"
alias bers="bundle exec rspec"

alias g="git"
alias c="code"

alias d="docker"
alias dc="docker compose"
