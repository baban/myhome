;;; .emacs -- my emacs cobfiguration
;; Author: babanba-n
;; Created: 10 Dec 2011
;; Keywords: docs

;; ロードパス追加
(setq load-path
  (append
    (list
      (expand-file-name "~/.site-lisp/"))
      load-path))

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


(when (featurep 'carbon-emacs-package)
  (progn
    ;; メタキーをoptionに変更
    (mac-key-mode 1)
    (setq mac-option-modifier 'meta)
    ;; carbon emacs でメタキーを altに変える
 
    (setq pc-select-selection-keys-only t)
    (pc-selection-mode 1)
    ;(set-language-environment 'Japanese)
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
    ;(set-face-foreground 'region "black")
    ;; 透明度の設定
    (set-frame-parameter nil 'alpha '(100 80))
    ;; タブ化
    (require 'elscreen)
    (define-key elscreen-map "\C-z" 'iconify-or-deiconify-frame)
    ;; linumを有効化
    (require 'linum)
    (global-linum-mode t)
    (setq linum-format "%5d ")
    (global-set-key [f5] 'linum-mode)))

;; macシステム
(when (eq system-type 'darwin) ())

;; 非mac で 非GUI な設定
(when (not (featurep 'carbon-emacs-package))
  (progn
    (require 'wb-line-number)
    (wb-line-number-toggle)))

;; git 対応
(require 'magit)

(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(define-key global-map "\M-?" 'help-for-help)        ; ヘルプ
(define-key global-map "\C-ci" 'indent-region)       ; インデント
(define-key global-map "\C-c\C-i" 'dabbrev-expand)   ; 補完
(define-key global-map "\C-c;" 'comment-region)      ; コメントアウト
(define-key global-map "\C-c:" 'uncomment-region)    ; コメント解除
(define-key global-map "\C-o" 'toggle-input-method)  ; 日本語入力切替
(define-key global-map "\C-\\" nil)                  ; \C-\の日本語入力の設定を無効にする
(define-key global-map "\C-c " 'other-frame)         ; フレーム移動
(define-key global-map "\M-g " 'goto-line)           ; 指定行へジャンプ

;; 拡張子tplを関連付け
(add-to-list 'auto-mode-alist '("//.tpl$" . html-mode))
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

