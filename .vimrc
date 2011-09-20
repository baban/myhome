" Commons
set nocompatible        " Use Vim defaults
set ruler               " show the cursor position all the time
set sw=2 ts=2 et sts=2  " tapping
syntax on
sy enable               " Listning syntax items
set history=50          " keep 50 lines of command line history
set hlsearch            " Highlight all matched items
set nows
set nu
set lsp=0               " Linspace: default 0, 1 for Win32 GUI
set lpl                 " Load plugins at startup
set wrap
set sel=exclusive
set wmnu
filetype plugin on
filetype indent on
set bs=indent,eol,start " allow backspacing over everything in insert mode
set visualbell t_vb=    " no beep, no flash
set t_Co=256            " 256 color support
set si
set incsearch           " timely show match keywords
set smartcase           " 検索で小文字なら大文字を無視、大文字なら無視しない設定

"set cursorline         " show current cursor line

set encoding=utf-8

" Tab
map tn :tabnext<CR>
map tp :tabprevious<CR>
map te :tabedit
map tc :tabclose<CR>
map tf :tabfirst<CR>
map tl :tablast<CR>

