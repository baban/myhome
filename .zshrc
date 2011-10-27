autoload -U colors; colors
autoload -U compinit; compinit

PROMPT=$'[%n@%m %1~]%% '
SPROMPT="correct: %R -> %r ? "

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# Emacsライクキーバインド設定
bindkey -e

limit coredumpsize 102400

#aliases           beep              cshjunkieloops   globassign           histnostore          listpacked       numericglobsort   promptsp         shoptionletters
#allexport         bgnice            cshjunkiequotes  globcomplete         histreduceblanks     listrowsfirst    octalzeroes       promptsubst      shortloops    
#alwayslastprompt  braceccl          cshnullcmd       globdots             histsavebycopy       listtypes        onecmd            promptvars       shwordsplit   
#alwaystoend       braceexpand       cshnullglob      globsubst            histsavenodups       localoptions     overstrike        pushdignoredups  singlecommand 
#appendhistory     bsdecho           debugbeforecmd   hashall              histsubstpattern     localtraps       pathdirs          pushdminus       singlelinezle 
#autocd            caseglob          dotglob          hashcmds             histverify           log              pathscript        pushdsilent      sourcetrace   
#autocontinue      casematch         dvorak           hashdirs             hup                  login            physical          pushdtohome      stdin         
#autolist          cbases            emacs            hashlistall          ignorebraces         longlistjobs     posixaliases      rcexpandparam    sunkeyboardhack
#automenu          cdablevars        equals           histallowclobber     ignoreeof            magicequalsubst  posixbuiltins     rcquotes         trackall      
#autonamedirs      chasedots         errexit          histappend           incappendhistory     mailwarn         posixcd           rcs              transientrprompt
#autoparamkeys     chaselinks        errreturn        histbeep             interactive          mailwarning      posixidentifiers  recexact         trapsasync    
#autoparamslash    checkjobs         evallineno       histexpand           interactivecomments  markdirs         posixjobs         rematchpcre      typesetsilent 
#autopushd         clobber           exec             histexpiredupsfirst  ksharrays            menucomplete     posixstrings      restricted       unset         
#autoremoveslash   combiningchars    extendedglob     histfcntllock        kshautoload          monitor          posixtraps        rmstarsilent     verbose       
#autoresume        completealiases   extendedhistory  histfindnodups       kshglob              multibyte        printeightbit     rmstarwait       vi            
#badpattern        completeinword    flowcontrol      histignorealldups    kshoptionprint       multifuncdef     printexitvalue    sharehistory     warncreateglobal
#banghist          correct           functionargzero  histignoredups       kshtypeset           multios          privileged        shfileexpansion  xtrace        
#bareglobqual      correctall        glob             histignorespace      kshzerosubscript     nomatch          promptbang        shglob           zle           
#bashautolist      cprecedences      globalexport     histlexwords         listambiguous        notify           promptcr          shinstdin                      
#bashrematch       cshjunkiehistory  globalrcs        histnofunctions      listbeep             nullglob         promptpercent     shnullcmd    

# 出力の文字列末尾に改行コードが無い場合でも表示
unsetopt promptcr

#allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

# 出力の文字列末尾に改行コードが無い場合でも表示
unsetopt promptcr

# 色を使う
setopt prompt_subst

# 補完候補一覧でファイルの種別をマーク表示
setopt list_types

# 履歴ファイルに時刻を記録
setopt extended_history

# 補完するかの質問は画面を超える時にのみに行う｡
LISTMAX=0

# =command を command のパス名に展開する
setopt equals

# --prefix=/usr などの = 以降も補完
setopt magic_equal_subst

# sudo でも補完の対象
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin /usr/sbin /usr/bin /sbin /bin

# ディレクトリ名だけで､ディレクトリの移動をする｡
setopt auto_cd

# 補完キー（Tab, Ctrl+I) を連打するだけで順に補完候補を自動で補完
setopt auto_menu

# cdのタイミングで自動的にpushd
setopt auto_pushd 

# 複数の zsh を同時に使う時など history ファイルに上書きせず追加
setopt append_history

# 補完候補が複数ある時に、一覧表示
setopt auto_list

# カッコの対応などを自動的に補完
setopt auto_param_keys

# ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash

# ビープ音を鳴らさないようにする
setopt no_beep

# スペルチェック
setopt correct

# C-s, C-qを無効にする。
setopt no_flow_control

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
# setopt no_list_types

# 保管結果をできるだけ詰める
setopt list_packed

# 補完候補一覧でファイルの種別をマーク表示
setopt list_types

# コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
setopt magic_equal_subst

# ファイル名の展開でディレクトリにマッチした場合末尾に / を付加する
setopt mark_dirs

# 8 ビット目を通すようになり、日本語のファイル名を表示可能
setopt print_eight_bit

# 同じディレクトリを pushd しない
setopt pushd_ignore_dups

# シェルのプロセスごとに履歴を共有
setopt share_history

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

# 環境ごとの独自設定を記述
case `uname` in 
  "Linux")   [ -f ~/.zshrc.linux  ] && source ~/.zshrc.linux;;
  "Darwin")  [ -f ~/.zshrc.mac    ] && source ~/.zshrc.mac;;
esac

# ホストサーバーごとの依存の設定を追記
[ -f ~/.zshrc.mine ] && source ~/.zshrc.mine

