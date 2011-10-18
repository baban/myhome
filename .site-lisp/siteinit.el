;;--------------------------------------------------
;; File name    :   siteinit.el
;;              :   Emacs の基本的な設定
;;              :
;;--------------------------------------------------
;;
;=======================================================================
; このファイルが置かれているディレクトリ
;=======================================================================
(defvar siteinit-path (file-name-directory (locate-library "siteinit")))

;;
;=======================================================================
; Language
;=======================================================================
(set-language-environment "Japanese")

;;
;=======================================================================
; OS ごとの設定を読み込む
;=======================================================================
(cond
  ;; Windows の NTEmacs23
  ((and (equal system-type 'windows-nt) (= emacs-major-version 23))
   (load (concat siteinit-path "ntemacs23")))
  ;; Linux の Emacs23
  ((= emacs-major-version 23)
   (load (concat siteinit-path "emacs23"))))
  ;)

;;
;=======================================================================
; elisp の追加読み込み PATH
;=======================================================================
(add-to-list 'load-path (concat siteinit-path "anything"))
(add-to-list 'load-path (concat siteinit-path "auto-complete"))
(add-to-list 'load-path (concat siteinit-path "magit"))
(add-to-list 'load-path (concat siteinit-path "mmm-mode"))

;;
;=======================================================================
; elisp の自動バイトコンパイル
;=======================================================================
(require 'auto-async-byte-compile)
;; 無視リスト
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;
;=======================================================================
; mmm-mode
; バッファ内で、複数のメジャーモードを共存
;
; - Project page
; http://mmm-mode.sourceforge.net/
;=======================================================================
;; mmm-mode
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face nil)
;(set-face-background 'mmm-default-submode-face "gainsboro")

;;
;=======================================================================
; isearch
;=======================================================================
;; isearch 中に、カーソル付近の文字を 1 文字ずつ追加
(defun isearch-yank-char ()
  "Pull next character from buffer into search string."
  (interactive)
  (isearch-yank-string
   (save-excursion
     (and (not isearch-forward) isearch-other-end
          (goto-char isearch-other-end))
     (buffer-substring (point) (1+ (point))))))

;; C-w で追加した後でも、一文字ずつ消す
(defun isearch-real-delete-char ()
  (interactive)
  (setq isearch-string
        (if (< (length isearch-string) 1)
            ""
          (substring isearch-string 0 (- (length isearch-string) 1)))
        isearch-message isearch-string
        isearch-yank-flag t)
  (isearch-search-and-update))

;; isearch で検索している単語で occur をする
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur
     (if isearch-regexp
         isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map "\C-f" 'isearch-yank-char)
(define-key isearch-mode-map "\C-h" 'isearch-real-delete-char)
(define-key isearch-mode-map "\C-l" 'isearch-edit-string)      ;C-lでキーワードの編集
(define-key isearch-mode-map "\C-o" 'isearch-occur)

;;
;=======================================================================
; dired
;=======================================================================

;; r -> C-x C-s でファイル名の編集
(require 'wdired)

;; スペースでマークする (FD like)
(defun dired-toggle-mark (arg)
  "Toggle the current (or next ARG) files."
  ;; S.Namba Sat Aug 10 12:20:36 1996
  (interactive "P")
  (let ((dired-marker-char
         (if (save-excursion (beginning-of-line)
                             (looking-at " "))
             dired-marker-char ?\040)))
    (dired-mark arg)
    ;(dired-previous-line 1)
    ))

(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode) ;rで編集モードに
(define-key dired-mode-map " " 'dired-toggle-mark)            ;スペースでマークをトグル

;;
;=======================================================================
; grep-edit.el
; - grep の検索結果を直接編集する
;
; - Project homepage
; http://www.bookshelf.jp/soft/meadow_51.html#SEC782
;=======================================================================
;; M-x grep 後バッファを編集
;;   - C-c C-c で変更を適用 (or C-c C-e)
;;   - C-c C-u で変更の破棄
;;   - C-c C-r でリージョン内の変更の破棄
(require 'grep-edit)

(defun my-grep-edit-setup ()
  ;; (define-key grep-mode-map '[up] nil)
  (define-key grep-mode-map "\C-c\C-c" 'grep-edit-finish-edit)
  (message (substitute-command-keys "\\[grep-edit-finish-edit] to apply changes."))
  (set (make-local-variable 'inhibit-read-only) t)
  )
(add-hook 'grep-setup-hook 'my-grep-edit-setup t)

;;
;=======================================================================
; flymake.el
; - 構文チェック
;=======================================================================
(require 'flymake)

;; GUI の警告表示を無効
(setq flymake-gui-warnings-enabled nil)

;; 色設定
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; ファイル名が有効の時に flymake-mode にする関数
(defun flymake-mode-if-enable-buffer ()
  (if (not (null buffer-file-name)) (flymake-mode)))

;; 汎用 flymake ルール
(defun flymake-simple-generic-init (cmd &optional opts)
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list cmd (append opts (list local-file)))))

;; flymake のエラー/警告をミニバッファに表示
(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

;-----------------------------------------------
; C, C++ で flymake を使う
;-----------------------------------------------

;; make 用の設定
(defun flymake-get-make-cmdline (source base-dir)
  (list "make"
        (list "-s"
              "-C" base-dir
              "LANG=C"               ; 警告を英語表示 (warning) させる
              (concat "CHK_SOURCES=" source)
              "SYNTAX_CHECK_MODE=1"
              "check-syntax")))

;; C 用の設定
;; (defun flymake-c-init ()
;;   (flymake-simple-generic-init
;;    "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

;; C++ 用の設定
;; (defun flymake-cc-init ()
;;   (flymake-simple-generic-init
;;    "g++" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

(add-hook 'c-mode-common-hook 'flymake-mode-if-enable-buffer)

;-----------------------------------------------
; ruby で flymake を使う
;-----------------------------------------------
(defun flymake-ruby-init ()
  (flymake-simple-generic-init
   "ruby" '("-c")))

(add-hook 'ruby-mode-hook 'flymake-mode-if-enable-buffer)
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

;;
;=======================================================================
; gdb-mode.el
; - デバッガ
;=======================================================================
;; いろんな情報を表示するバッファを開く
(setq gdb-many-windows t)

;; I/O バッファを開くかどうか
(setq gdb-use-separate-io-buffer t)

;;
;=======================================================================
; ruby-mode.el
; - ruby プログラミング用のメジャーモード
;
; - インストール
; # aptitude install ruby-elisp
;
; - Project homepage
; http://www.ruby-lang.org/ja/
;=======================================================================
(when (locate-library "ruby-mode")
  ;; ruby-mode
  (autoload 'ruby-mode "ruby-mode" nil t)

  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

  ;; ruby-electric - 閉じ括弧とか end を保管する
  (when (locate-library "ruby-electric")
    (require 'ruby-electric)
    (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t))))

  ;; inf-ruby - irb 実行環境
  (when (locate-library "inf-ruby")
    (autoload 'run-ruby "inf-ruby" nil)
    (autoload 'inf-ruby-keys "inf-ruby" nil)
    (add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys))))

  ;; rubydb - ruby デバッガ
  (when (locate-library "rubydb3x")
    (autoload 'rubydb "rubydb3x" nil t))
  )

;;
;=======================================================================
; php-mode.el
; - php プログラミング用のメジャーモード
;
; - Project homepage
; http://sourceforge.net/projects/php-mode/
;=======================================================================
(require 'php-mode)

(add-hook 'php-mode-hook
          '(lambda ()
             ;; インデントスタイルの設定(gnu,bsd,k&r,stroustrup,linux,java)
             (c-set-style "stroustrup")
             ;; 連続する空白の一括削除
             (c-toggle-hungry-state t)
             ;; コメント行のインデント
             (setq c-comment-only-line-offset 0)
             ;; コメントのスタイル
             (setq comment-start "// "
                   comment-end   ""
                   comment-start-skip "// *")
             ;; `;' を入力したら、自動改行+インデント
             (c-toggle-auto-hungry-state 1)
             ;; インデント サイズの設定
             (setq tab-width 4
                   c-basic-offset 4
                   c-hanging-comment-ender-p nil
                   indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;;
;=======================================================================
; minibuf-isearch.el
; - ミニバッファで isearch 的な検索をする
;
; - Project page
; http://www.sodan.org/~knagano/emacs/minibuf-isearch/
;=======================================================================
(require 'minibuf-isearch)
;; minibuf-isearch 中は migemo を利用しない
(setq minibuf-isearch-use-migemo nil)

;;
;=======================================================================
; session.el
; - ミニバッファの履歴をファイルに保存し、次回起動時にも持ち越せる
;
; - Project page
; http://emacs-session.sourceforge.net/
;=======================================================================
(require 'session)
(setq session-initialize '(de-saveplace session keys menus places)
      session-globals-include '((kill-ring 50)
                                (session-file-alist 500 t)
                                (file-name-history 10000)))
;; これがないと file-name-history に500個保存する前に max-string に達する
(setq session-globals-max-string 100000000)
;; デフォルトでは30!
(setq history-length t)
(add-hook 'after-init-hook 'session-initialize)

;;
;=======================================================================
; kill-summary.el
; - kill-ring の内容を一覧表示してそこから yank する
;
; - Project page
; http://emacs-session.sourceforge.net/
;=======================================================================
(autoload 'kill-summary "kill-summary" nil t)

;;
;=======================================================================
; gtags.el
; - GNU Global を使ったタグジャンプ
;
; - Project page
; http://www.gnu.org/software/global/
;=======================================================================
(when (locate-library "gtags")
  (require 'gtags))

;; *GTAGS SELECT* バッファを一つしか生成しないようにする
(setq gtags-select-buffer-single t)

;; gtags を使用するモード
(add-hook 'java-mode-hook (lambda () (gtags-mode 1)))
(add-hook 'c-mode-hook (lambda () (gtags-mode 1)))
(add-hook 'c++-mode-hook (lambda () (gtags-mode 1)))

;;
;=======================================================================
; tabbar.el
; - バッファリストのタブ表示
;
; - インストール
; # aptitude install emacs-goodies-el
;
; - Project wiki
; http://www.emacswiki.org/cgi-bin/wiki/TabBarMode
;=======================================================================
(when (require 'tabbar nil t)

  ;; タブをグループ化しない
  (setq tabbar-buffer-groups-function
        (lambda () (list "All Buffers")))

  ;; `*scratch*' 以外の ` *'から始まるバッファーをリストしない
  (setq tabbar-buffer-list-function
        (lambda ()
          (remove-if
           (lambda (buffer)
             (unless (string-match-p "\\*\\(scratch\\|tmp\\)" (buffer-name buffer))
               (find (aref (buffer-name buffer) 0) " *")))
           (buffer-list))))

  ;; tabbar のバージョン違いごとの設定
  (cond ((string= tabbar-version "2.0")
         ;; Ubuntu 付属の tabbar
         ;;
         ;; 左端のボタンを無効化
         (setq tabbar-home-button nil)
         (setq tabbar-buffer-home-button nil)
         (setq tabbar-scroll-left-button nil)
         (setq tabbar-scroll-right-button nil)

         ;; 色設定
         (set-face-attribute 'tabbar-default nil :background "gray60")
         (set-face-attribute 'tabbar-unselected nil :background "gray85" :foreground "gray30" :box nil)
         (set-face-attribute 'tabbar-selected nil :background "#f2f2f6" :foreground "red" :box nil)
         (set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "gray72" :style released-button))

         ;; 幅設定
         (setq tabbar-separator (list 0.5)))

        (t
         ;; NTEmacs 用 (tabbar-version 1.3)
         ;;
         ;; 左端のボタンを無効化
         (setq tabbar-home-button-enabled "")
         (setq tabbar-scroll-right-button-enabled "")
         (setq tabbar-scroll-left-button-enabled "")
         (setq tabbar-scroll-right-button-disabled "")
         (setq tabbar-scroll-left-button-disabled "")

         ;; 色設定
         (set-face-attribute 'tabbar-default-face nil :background "gray60")
         (set-face-attribute 'tabbar-unselected-face nil :background "gray85" :foreground "gray30" :box nil)
         (set-face-attribute 'tabbar-selected-face nil :background "#f2f2f6" :foreground "red" :box nil)
         (set-face-attribute 'tabbar-button-face nil :box '(:line-width 1 :color "gray72" :style released-button))

         ;; 幅設定
         (set-face-attribute 'tabbar-separator-face nil :height 0.7)))

  ;; tabbar を有効にする
  (tabbar-mode))

;=======================================================================
; setnu.el, setnu+.el
; - 行番号表示モード
;
; - Project wiki
; http://www.emacswiki.org/emacs/LineNumbers#toc9
;=======================================================================
(autoload 'setnu-mode "setnu+" nil t)

;;
;=======================================================================
; yasnippet.el
; - 入力支援
;
; - Project homepage
; http://code.google.com/p/yasnippet/
;=======================================================================
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat siteinit-path "snippets"))

;;
;=======================================================================
; anything.el
; - ファイルを開くを一元化
;
; - Project wiki
; http://www.emacswiki.org/emacs/Anything/
;=======================================================================
(require 'anything-config)

;; 情報元を設定
(setq anything-sources (list anything-c-source-buffers
                             anything-c-source-bookmarks
                             anything-c-source-file-name-history
                             anything-c-source-man-pages
                             anything-c-source-info-pages
                             anything-c-source-complex-command-history))

;; anything のキーマップ
(define-key anything-map "\C-p" 'anything-previous-line)
(define-key anything-map "\C-n" 'anything-next-line)
(define-key anything-map "\C-v" 'anything-next-page)
(define-key anything-map "\M-v" 'anything-previous-page)

;;
;=======================================================================
; test-case-mode.el
; - テスト駆動開発支援
;
; - Project Homepage
; http://nschum.de/src/emacs/test-case-mode/
;=======================================================================
(require 'test-case-mode)
(add-hook 'find-file-hook 'enable-test-case-mode-if-test)

;; コンパイル後に、テストの自動実行
(add-hook 'compilation-finish-functions 'test-case-compilation-finish-run-all)

;; test-case-mode のキーマップ
(define-key test-case-mode-map "\C-ct" 'test-case-run)

;;
;=======================================================================
; dabbrev-ja.el
; - 日本語で dabbrev(動的略語補完) を使う
;=======================================================================
;(load "dabbrev-ja")

;;
;=======================================================================
; redo+.el
; - undo, redo を行えるようにする
;
; http://www11.atwiki.jp/s-irie/pages/18.html
;=======================================================================
(require 'redo+)
(setq undo-no-redo t)

;;
;=======================================================================
; auto-complete.el
; - 自動保管
;
; - Project wiki
; http://www.emacswiki.org/emacs/AutoComplete
;=======================================================================
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat siteinit-path "auto-complete/dict"))
(ac-config-default)

;; 4 文字以上の単語の時に補完を開始する
(setq ac-auto-start 4)

;; キーマップ
(define-key ac-completing-map "\M-/" 'ac-stop) ; 保管の停止

;;
;=======================================================================
; magit.el
; - Git
;
; - Project page
; http://philjackson.github.com/magit/
;=======================================================================
(autoload 'magit-status "magit" nil t)

;;
;=======================================================================
; mercurial.el
; - Mercurial 付属の elisp
;
; C-c h h でキーバインディング一覧が見れる
;
; - 分かりやすい help
; http://www.lares.dti.ne.jp/~foozy/fujiguruma/scm/mercurial-emacs.html
;=======================================================================
(when (require 'mercurial nil t)
  ;; MQ (Mercurial Queue) で patch 管理をする
  (require 'mq nil t))

;;
;=======================================================================
; c-mode, c++-mode
;
; - 参考
; http://d.hatena.ne.jp/i_s/20091026/1256557730
;=======================================================================
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; インデント設定
(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; インデントスタイルの設定(gnu,bsd,k&r,stroustrup,linux,java)
             (c-set-style "stroustrup")
             ;; インデントに使う文字設定(t: タブ, nil: スペース)
             (setq indent-tabs-mode nil)
             ;; インデント幅
             (setq c-basic-offset 4)
             ;; namespace {}の中はインデントしない
             (c-set-offset 'innamespace 0)
             ;; 関数の引数リストの閉じ括弧はインデントしない
             (c-set-offset 'arglist-close 0)
             ;; インライン関数の開始括弧はインデントしない
             (c-set-offset 'inline-open 0)
             ;; メンバ初期化リストの開始 `:' のインデント量
             (c-set-offset 'member-init-intro 2)
             ;; extern "C" 内のブロックのインデント量
             (c-set-offset 'inextern-lang 0)
             ;; `;' を入力したら、自動改行+インデント
             (c-toggle-auto-hungry-state 1)
             ;; Enterで改行とインデント
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)
             ;; ソースとヘッダの切り替え
             (define-key c-mode-base-map [f4] 'ff-find-other-file)
            ))

;; ff-find-other-file でヘッダを探すパス
(defcustom cc-search-directories
  '("." "/usr/include" "/usr/local/include/*")
  "*See the description of the `ff-search-directories' variable."
  :type '(repeat directory)
  :group 'ff)

;;
;=======================================================================
; kanji-code.el
; - 日本語と Escaped Unicode を変換する
;=======================================================================
(autoload 'kanji-to-unicode-buffer "kanji-code" nil t)
(autoload 'kanji-to-unicode-region "kanji-code" nil t)
(autoload 'unicode-to-kanji-buffer "kanji-code" nil t)
(autoload 'unicode-to-kanji-region "kanji-code" nil t)

;;
;=======================================================================
; actionscript-mode
;
; - Project wiki
; http://www.emacswiki.org/emacs/ActionScriptMode/
;=======================================================================
(autoload 'actionscript-mode "actionscript-mode" "actionscript" t)
(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.mxml\\'" . xml-mode))

;; mxml + Action Script
(mmm-add-classes
 '((embedded-as
    :submode actionscript-mode
    :face mmm-code-submode-face
    ;:front "<mx:Script[^>]*>\\n?\\s-*<!\\[CDATA\\["
    ;:back "]]>\\n?\\s-*</mx:Script>")))
    :front "<mx:Script>"
    :back "</mx:Script>")))
(mmm-add-mode-ext-class nil "\\.mxml\\'" 'embedded-as)

;;
;=======================================================================
; twittering-mode
;
; - Project wiki
; http://www.emacswiki.org/emacs/TwitteringMode/
;=======================================================================
(require 'twittering-mode)
(setq twittering-icon-mode t)                 ; Show icons

;;
;=======================================================================
; スクロール設定
;=======================================================================
;; スクロールの基本設定
(setq scroll-conservatively 15                ;画面の下端 (上端) で移動したときのスクロール量
      scroll-step 1                           ;(同上)
      scroll-margin 0)                        ;まともに動かない

;; ホイールマウスでスクロールを有効に
(mouse-wheel-mode t)

;; ホイールマウスのスクロール幅を設定（画面の８分の１）
(global-set-key [mouse-4] '(lambda () (interactive) (scroll-down (/ (window-height) 8))))
(global-set-key [mouse-5] '(lambda () (interactive) (scroll-up (/ (window-height) 8))))

;; スクロール時にカーソル位置を変えない
(setq scroll-preserve-screen-position t)

;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)

;;
;=======================================================================
; 一般拡張
;=======================================================================
(setq make-backup-files nil)                   ;バックアップファイルを作成しない
(setq auto-save-timeout 30)                    ;自動保存する間隔（秒）。
(setq auto-save-interval 300)                  ;300打鍵ごとに自動保存
(setq delete-auto-save-files t)                ;終了時にオートセーブファイルを消す
(setq visible-bell t)                          ;警告音を消す
(setq kill-whole-line t)                       ;カーソルが行頭にある場合は行全体を削除
(setq ring-bell-function 'ignore)              ;エラー音をならなくする
(setq delete-by-moving-to-trash t)             ;ごみ箱を有効
(setq-default indent-tabs-mode nil)            ;インデントにはスペースを使う
(setq require-final-newline t)                 ;ファイル末尾に改行追加

;; インデントに使用する関数を指定
(setq indent-line-function 'indent-relative-maybe)

;; yankのシステムへのコピー
(cond (window-system
       (setq x-select-enable-clipboard t)
       ))

;; ファイル名や URL の上で C-x C-f すると、そのファイル名や URL が開ける
(ffap-bindings)

;; Shift+カーソルキー で、分割したウィンドウ間を移動
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;;
;=======================================================================
; 表示設定
;=======================================================================
(auto-compression-mode t)                      ;日本語infoの文字化け防止
(auto-image-file-mode t)                       ;画像ファイルを表示
(blink-cursor-mode 0)                          ;カーソルを点滅しないように
(global-font-lock-mode t)                      ;font-lockを有効
(set-scroll-bar-mode 'right)                   ;スクロールバーを右側に表示
(setq inhibit-startup-message t)               ;起動時の画面はいらない
(setq parse-sexp-ignore-comments t)            ;コメント内の括弧は無視
(setq show-paren-style 'mixed)                 ;対応する括弧がウィンドウ内に収まらないときに光らせる
(setq tab-stop-list '(4 8 12 16 20 24 28 32    ;タブストップ位置の設定
                        36 40 48 52 56 60 64
                        68 72 76 80))
(setq-default indicate-empty-lines t)          ;ファイルの終端以降を可視化
(setq-default indicate-buffer-boundaries 'right);右フリンジにバッファの開始と終端を表示
(setq-default tab-width 4)                     ;タブ幅を4に設定
(show-paren-mode t)                            ;対応する括弧をハイライト
(tool-bar-mode 0)                              ;ツールバーを表示しない

;; フレームのタイトル指定
(setq frame-title-format
      (concat "%b - emacs@" system-name))

;-----------------------------------------------
; 配色
;-----------------------------------------------

;; リージョン
(set-face-foreground 'region "black")
(set-face-background 'region "cyan2")
;; モードライン
(set-face-foreground 'mode-line "#000000")
(set-face-background 'mode-line "grey")
(set-face-attribute 'mode-line nil :box nil)   ;モードラインを平面化
(set-face-foreground 'mode-line-inactive "#000000")
(set-face-background 'mode-line-inactive "grey")
(set-face-attribute 'mode-line-inactive nil :box nil)

;-----------------------------------------------
; バッファのデフォルト表示設定
;-----------------------------------------------
(setq default-frame-alist
      (append (list
               '(top . 0)                      ;Y 表示位置
               '(left . 0)                     ;X 表示位置
               '(width . 110)                  ;フレームの幅
               '(height . 60)                  ;フレームの高さ
               '(alpha . 90)                   ;透明度
               '(foreground-color . "white")   ;文字色
               '(background-color . "#222222") ;背景色
               '(cursor-type  . box)           ;カーソルのタイプ
               '(cursor-color . "cyan2")       ;カーソル色
               '(border-color . "black")
               '(mouse-color . "white")
               )
              default-frame-alist))
(setq initial-frame-alist default-frame-alist)

;-----------------------------------------------
; モードライン
;-----------------------------------------------
;; 時計を表示
(setq display-time-24hr-format t)
(setq display-time-string-forms '(24-hours ":" minutes))
(display-time-mode t)

;; 行や列数号を表示
(line-number-mode t)
(column-number-mode t)

;-----------------------------------------------
; タブや全角スペース、行末のスペースを表示する
;
; - 行末のスペースは次のコマンドで削除できる
; M-x delete-trailing-whitespace
;-----------------------------------------------
(require 'whitespace)
(setq whitespace-style '(tabs tab-mark
                         spaces space-mark
                         newline newline-mark
                         ))
(setq whitespace-space-regexp "\\(\u3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])            ;全角スペース
        (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])      ;タブ
        (newline-mark ?\n [?\x21B5 ?\n] [?$ ?\n]) ;改行
        ))
(set-face-background 'whitespace-space 'nil)
(set-face-background 'whitespace-tab 'nil)
(global-whitespace-mode t)

;上記のwhitespace-modeの改行表示とは共存不可
;(setq-default show-trailing-whitespace t)
;(set-face-attribute 'trailing-whitespace nil
;                    :foreground "SteelBlue"
;                    :background "#222222"
;                    :underline t)

;-----------------------------------------------
; カーソル行をハイライト
;-----------------------------------------------
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "#004422"))
    (((class color)
      (background light))
     (:background "SkyBlue"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)  ; or 'underline
(global-hl-line-mode)

;;
;=======================================================================
; バッファ設定
;=======================================================================

;; バッファの切り替えに iswitchb を使う
(iswitchb-mode t)

;; C-xb 後の C-s, C-r で、プレビュー表示する
(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "選択している buffer を window に表示してみる。"
  (when (and
         (eq iswitchb-method iswitchb-default-method)
         iswitchb-matches)
    (select-window
     (get-buffer-window (cadr (buffer-list))))
    (let ((iswitchb-method 'samewindow))
      (iswitchb-visit-buffer
       (get-buffer (car iswitchb-matches))))
    (select-window (minibuffer-window))))

(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(defun my-buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))

(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my-make-scratch 0) nil)
                        t))))

(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (function (lambda ()
                      (unless (member "*scratch*" (my-buffer-name-list))
                        (my-make-scratch 1)))))

;;
;=======================================================================
; schema-mode using gauche
;=======================================================================
(setq scheme-program-name "gosh -i")
(autoload 'schema-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-mode "cmuscheme" "Run an inferior Scheme process." t)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (split-window-horizontally)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name)
  (other-window -1))

;;
;=======================================================================
; function
;=======================================================================

;; カレントバッファを閉じる
(defun my-kill-buffer (all)
  (interactive "P")
  (if all
      ;prefix argument があれば全バッファを削除
      (loop for buffer being the buffers
            do (kill-buffer buffer))
    (kill-buffer nil)))

;; カレントバッファを閉じる
(defun my-revert-buffer (&optional coding-system)
  (interactive "zCoding system for visited file (default nil): \nP")
  (if coding-system
      ;prefix argument があればエンコード指定
      ;(revert-buffer-with-coding-system coding-system)
      t
    (revert-buffer t t)))

;; 作業用バッファを作る
(defvar tmp-buffer-count 0 "new bufferを作った数")
(defun create-tmp-buffer ()
  (interactive)
  (switch-to-buffer (if (= tmp-buffer-count 0)
                        "*tmp*"
                        ;TODO: 動的に数値を取得する
                        (concat "*tmp (" (number-to-string tmp-buffer-count) ")*")))
  (setq tmp-buffer-count (+ 1 tmp-buffer-count)))

;; カーソル位置の単語を検索
(defun search-word-cursor ()
  (interactive)
  (if (thing-at-point 'symbol)
      (occur (thing-at-point 'symbol))
    (call-interactively 'occur)))

;; 二分割されている画面を入れ替える
(defun swap-screen ()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))

;; 二分割されている画面、カーソルを入れ替える
(defun swap-screen-with-cursor ()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))

;; ウィンドウ二分割時に、縦分割<->横分割
(defun window-toggle-division ()
  "ウィンドウ 2 分割時に、縦分割<->横分割"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "ウィンドウが 2 分割されていません。"))
  (let (before-height (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally)
      )
    (switch-to-buffer-other-window other-buf)
    (other-window -1)))

;; 対応する括弧に移動する
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;        (t (self-insert-command (or arg 1)))))
        ))

;; タブ幅の設定
(defun set-tab-width (num)
  (interactive "nTab Width: ")
  (make-local-variable 'tab-stop-list)
  (setq tab-width num)
  (setq tab-stop-list ())
  (while (< num 128)
    (add-to-list 'tab-stop-list num t)
    (setq num (+ num tab-width))))

;; ウィンドウが分割されていたら次のウィンドウに移動、そうでなければ分割する
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

;; 分割したウィンドウのリサイズ
(defun my-window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

;; 折り返し表示をトグルする
(defun toggle-truncate-lines ()
  "折り返し表示をトグルする."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil
            truncate-partial-width-windows nil)
    (setq truncate-lines t
          truncate-partial-width-windows t))
  (recenter))

;;----------------------------------------------
;; カーソル行を上下に移動
;;----------------------------------------------
; 下に移動
(defun transpose-lines-down (&optional n)
  "カーソル行を下に移動"
  (interactive "p")
  (if (not n)
      (setq n 1))
  (when (save-excursion (forward-line n))
    (let ((column (current-column))
          (beg (progn (move-beginning-of-line 1) (point)))
          (end (progn (forward-line 1) (point))))
      (insert (prog1
                  (buffer-substring beg end)
                (delete-region beg end)
                (forward-line n)))
      (forward-line -1)
      (move-to-column column))))

; 上に移動
(defun transpose-lines-up (&optional n)
  "カーソル行を上に移動"
  (interactive "p")
  (if (not n)
      (setq n 1))
  (transpose-lines-down (- n)))

;;
;=======================================================================
; キーカスタマイズ
;=======================================================================

(global-set-key "\C-cl" 'toggle-truncate-lines);折り返し表示のトグル
(global-set-key "\C-h" 'backward-delete-char)  ;バックスペース
;(global-set-key "\C-i" 'auto-complete)         ;文字列保管
(global-set-key [zenkaku-hankaku]
                'toggle-input-method)          ;日本語入力
(global-set-key "\C-o" 'toggle-input-method)   ;日本語入力
(global-set-key "\C-s" 'isearch-forward-regexp);正規表現で検索
(global-set-key "\C-r" 'query-replace-regexp)  ;正規表現で置換
(global-set-key "\C-x\C-g" 'my-revert-buffer)  ;カレントバッファを再読み込み
(global-set-key "\C-xa" 'anything)             ;anything の開始
(global-set-key "\C-xk" 'my-kill-buffer)       ;カレントバッファを閉じる
(global-set-key "\C-xt" 'create-tmp-buffer)    ;作業用バッファを作る
(global-set-key "\C-x\C-b" 'buffer-menu)       ;ウィンドウ分割しないバッファメニュー
(global-set-key "\C-\\" 'undo)                 ;undo
(global-set-key [?\C-:] 'search-word-cursor)   ;カーソル下の単語で検索
(global-set-key "\C-]" 'match-paren)           ;対応する括弧に移動
(global-set-key "\C-\M-y" 'kill-summary)       ;kill-ring 一覧から yank
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0))) ;;行頭まで削除
(global-set-key "\M-g" 'goto-line)             ;指定行へ移動
;; 半ページ/スクロール
(global-set-key "\M-]" '(lambda () (interactive) (scroll-up (/ (window-height) 2))))
(global-set-key "\M-[" '(lambda () (interactive) (scroll-down (/ (window-height) 2))))

(global-set-key [M-up] 'transpose-lines-up)    ;カーソル行を上に移動
(global-set-key [M-down] 'transpose-lines-down);カーソル行を下に移動
(global-set-key [f1] 'help-for-help)           ;ヘルプ
(global-set-key [f2] 'tabbar-backward-tab)     ;前のタブへ
(global-set-key [S-f2] 'tabbar-backward-group) ;前のタブグループへ
(global-set-key [f3] 'tabbar-forward-tab)      ;次のタブへ
(global-set-key [S-f3] 'tabbar-forward-group)  ;次のタブグループへ
(global-set-key [f7] 'next-error)              ;次のエラーを検索
(global-set-key [S-f7] 'previous-error)        ;前のエラーを検索
(global-set-key [f10] 'gtags-find-tag-from-here);タグジャンプ
(global-set-key [S-f10] 'gtags-pop-stack)      ;バックタグジャンプ
(global-set-key [f12] 'redo)                   ;redo
;(global-set-key [f12] 'setnu-mode)             ;行番号表示
(global-set-key [C-prior] 'tabbar-backward-tab);前のタブへ
(global-set-key [C-next] 'tabbar-forward-tab)  ;次のタブへ
(global-set-key [C-tab] 'other-window-or-split);次のウィンドウか分割か

(global-set-key "\C-cg" 'magit-status)         ;magit実行
(global-set-key "\C-cs" 'scheme-other-window)  ;scheme実行
(global-set-key "\C-cr" 'my-window-resizer)    ;ウィンドウのリサイズ

(put 'set-goal-column 'disabled nil)
