; -*- tab-width: 2 -*-
;;; .emacs -- my emacs cobfiguration
;; Author: babanba-n
;; Created: 10 Dec 2011
;; Keywords: docs

;; ロードパス追加
(setq load-path
  (append
    (list
      (expand-file-name "~/.site-lisp/")
      (expand-file-name "~/.emacs.d/elisp")
      )
      load-path))

(require 'server)
(unless (server-running-p)
    (server-start))

;; まず、install-elisp のコマンドを使える様にします。
;(require 'install-elisp)
;; 次に、Elisp ファイルをインストールする場所を指定します。
;(setq install-elisp-repository-directory "~/.emacs.d/elisp/")

;; 初期フレームの設定
(setq initial-frame-alist
  (append
     '((top    . 35)    ; フレームの Y 位置(ピクセル数)
       (left   . 100)   ; フレームの X 位置(ピクセル数)
       (width  . 120)   ; フレーム幅(文字数)
       (height . 30))   ; フレーム高(文字数)
        initial-frame-alist))

;; 新規フレームのデフォルト設定
(setq default-frame-alist
  (append
    '((width  . 120)	; フレーム幅(文字数)
      (height . 30))	; フレーム高(文字数)
       default-frame-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-linum-mode t)
 '(show-paren-mode t))

(when (featurep 'carbon-emacs-package)
  (progn
    ;; メタキーをoptionに変更
    (mac-key-mode 1)
    (setq mac-option-modifier 'meta)
    ;; carbon emacs でメタキーを altに変える
    (setq pc-select-selection-keys-only t)
    (pc-selection-mode 1)
    ;; アンチエイリアス
    (setq mac-allow-anti-aliasing nil)
    ;; 幅3ポイントの縦棒カーソル
    ;(add-to-list 'default-frame-alist '(cursor-type . '(bar . 3)))
    ;; カーソルの色を設定します。
    (add-to-list 'default-frame-alist '(cursor-color . "SlateBlue2"))
    ;; マウスポインタの色を設定します。
    (add-to-list 'default-frame-alist '(mouse-color . "SlateBlue2"))
    ;; 選択範囲の色を指定
    (set-face-background 'region "lavender")
    ;; 透明度の設定
    (set-frame-parameter nil 'alpha '(100 80))
    ;; タブ化
    (require 'elscreen)
    (define-key elscreen-map "\C-z" 'iconify-or-deiconify-frame)))

;; macシステム
(when (eq system-type 'darwin) ())

;; 
; mac フォント設定
(if (eq window-system 'mac) 
  ;; Monaco 12pt をデフォルトにする
  (set-face-attribute 'default nil
    :family "Monaco"
    :height 120)
  ;; 日本語をヒラギノ角ゴProNにする
  (set-fontset-font "fontset-default"
    'japanese-jisx0208
    '("Hiragino Maru Gothic ProN"))
  ;; 半角カナをヒラギノ角ゴProNにする
  (set-fontset-font "fontset-default"
    'katakana-jisx0201
    '("Hiragino Maru Gothic ProN")))

;; 非mac で 非GUI な設定
(when (not (featurep 'carbon-emacs-package)))

;; タブ化
(when (require 'tabbar nil t)
    (tabbar-mode))

;; タブや空白の表示設定
(defface my-face-b-1 '((t (:foreground "Red" :underline t))) nil)
(defface my-face-b-2 '((t (:foreground "gainsboro" :underline t))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ ]+$" 0 my-face-u-1 append))))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
  (if font-lock-mode nil (font-lock-mode t))) t)


;;タブは2文字ごとに
(setq-default tab-width 4)
(setq default-tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
(defun set-aurora-tab-width (num &optional local redraw)
  "タブ幅をセットします。タブ5とかタブ20も設定できたりします。
localが non-nilの場合は、カレントバッファでのみ有効になります。
redrawが non-nilの場合は、Windowを再描画します。"
  (interactive "nTab Width: ")
  (when local
    (make-local-variable 'tab-width)
    (make-local-variable 'tab-stop-list))
  (setq tab-width num)
  (setq tab-stop-list ())
  (while (<= num 256)
    (setq tab-stop-list `(,@tab-stop-list ,num))
    (setq num (+ num tab-width)))
  (when redraw (redraw-display)) tab-width)

(set-aurora-tab-width (setq default-tab-width (setq-default tab-width 8)))


;; git 対応
;(require 'magit)

;; zencoding対応
(require 'zencoding-mode)
(add-hook 'xml-mode-hook 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode)
(define-key global-map "\M-0" 'zencoding-expand-line)
;(define-key zencoding-mode-keymap "\M-0" 'zencoding-expand-line)

;; 拡張子tplを関連付け
(add-to-list 'auto-mode-alist '("\\.tpl$" . html-mode))
(show-paren-mode 1)

;; バックアップファイルは作らない
(setq backup-inhibited t)
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; 補完時に大文字小文字を区別しない
;(setq completion-ignore-case t)

;; 強力な補完機能を使う
; p-bでprint-bufferとか
(load "complete")
(partial-completion-mode 1)

;; 補完可能なものを随時表示
;; 少しうるさい
(icomplete-mode 1)

;; カーソルの位置が何文字目かを表示する
(column-number-mode t)

;; カーソルの位置が何行目かを表示する
(line-number-mode t)

; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;; バッファの最後でnewlineで新規行を追加するのを禁止する
;(setq next-line-add-newlines nil)

;; 最終行に必ず一行挿入する
(setq require-final-newline t)

;; 一行が 80 字以上になった時には自動改行する
;(setq fill-column 80)
;(setq-default auto-fill-mode t)

;; SCSS関連付け
;(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

;; 現在の関数名をモードラインに表示
(which-function-mode 1)

;; 最近使ったファイルを保存(M-x recentf-open-filesで開く)
(recentf-mode)

;; PHPモード
(autoload 'php-mode "php-mode" "PHP mode" t)
(defcustom php-file-patterns (list "\\.php\\'" "\\.inc\\'" "\\.ctp\\'")
  "*List of file patterns for which to automatically invoke php-mode."
  :type '(repeat (regexp :tag "Pattern"))
  :group 'php)
  (let ((php-file-patterns-temp php-file-patterns))
    (while php-file-patterns-temp
      (add-to-list 'auto-mode-alist
                 (cons (car php-file-patterns-temp) 'php-mode))
    (setq php-file-patterns-temp (cdr php-file-patterns-temp))))

;; 構文チェック
(add-hook 'php-mode-hook
  '(lambda ()
     (local-set-key "\C-ctj" 'php-lint)))

(defun php-lint ()
  "Performs a PHP lint-check on the current file."
  (interactive)
  (shell-command (concat "php -l " (buffer-file-name))))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'cl)
(defun close-all-buffers ()
  (interactive)
  (loop for buffer being the buffers
                 do (kill-buffer buffer)))

