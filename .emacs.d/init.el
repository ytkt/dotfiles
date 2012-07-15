;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
	(dolist (path paths paths)
	  (let ((default-directory
			  (expand-file-name (concat user-emacs-directory path))))
		(add-to-list 'load-path default-directory)
		(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
			(normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;; init-loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf")

;; スタートアップページを表示しない
(setq inhibit-startup-message t)


;; バックアップファイルを作らない
;; (setq backup-inhibited t)


;; フォントの大きさを "C-x C-+ or C--" で変更
(global-set-key [(control ?+)] (lambda () (interactive)
  (text-scale-increase 1)))
(global-set-key [(control ?-)] (lambda () (interactive)
  (text-scale-decrease 1)))
(global-set-key [(control ?0)] (lambda () (interactive)
  (text-scale-increase 0)))


;; "C-t" でウィンドウを切り替える
(define-key global-map (kbd "C-t") 'other-window)


;; "C-h" でバックスペース
(global-set-key "\C-h" 'delete-backward-char)


;; "C-m" にnewline-and-indentを割り当てる
(define-key global-map (kbd "C-m") 'newline-and-indent)


;; 文字コードの指定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)


;;; フレームに関する設定 ------------------------------
;; カラム番号も表示
(column-number-mode t)

;; ファイルサイズを表示
(size-indication-mode t)

;; 時計を表示
(setq display-time-24hr-format t)
(display-time-mode t)

;; バッテリー残量を表示
; (display-battery-mode t)

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;; 行番号を常に表示する
(global-linum-mode t)
lasetq linum-format "%4d ")


;;; インデントの設定 ------------------------------
;; タブキーのインデント
(setq default-tab-width 4)

;; インデントにタブ文字を使用しない
(setq indent-line-function 'indent-relative-maybe)


;;; 表示・修飾に関する設定 ------------------------------
(when (require 'color-theme nil t)
  ;; テーマを読み込むための設定
  (color-theme-initialize)
  ;; テーマをhoberに変更する
  (color-theme-hober))

;; シンタックスハイライトの設定
(defface my-hl-line-face
  ;; 背景がdarkならば背景色を紺に
  '((((class color) (background dark))
	 (:backgrounnd "NavyBlue" t))
	;; 背景色がlightならば背景色を緑に
	(((class color) (background light))
	 (:background "LightFoldenrodYellow"))
	(t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; 対応する括弧のハイライト
(setq show-paren-dilay 0)
(show-paren-mode t)
;; parenのスタイル: expressionは括弧内も強調表示
;;(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
;; オートセーブファイルの作成場所をシステムのTempディレクトリに変更する
;(setq auto-sabe-file-name-transforms
;	  `((".*" ,temporary-file-directory t)))
;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
			 (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
	  `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; オートセーブファイル作成までの秒間隔
(setq auto-save-timeout 15)
;; オートセーブファイル作成までのタイプ間隔
(setq auto-save-interval 60)




;;; フックの設定 ------------------------------------------------------------
;; ファイルが #! から始まる場合、+xをを付けて保存する
(add-hook 'after-save-hook
		  'executable-make-buffer-file-executable-if-script-p)

;; emacs-lisp-modeのフックをセット
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
	(setq eldoc-idle-delay 0.2)
	(setq eldoc-echo-area-use-multiline-p t)
	(turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)



;;; auto-installの設定 ------------------------------------------------------------
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する 初期値は~/.emacs.d/auto-install
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されている elisp の名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; install-elip の関数を利用可能にする
  (auto-install-compatibility-setup))

;; redo+の設定
(when (require 'redo nil t)
  ;; C-. にredoを割り当てる
  (global-set-key (kbd "C-.") 'redo))



;;; Anything
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間
   anything-idle-delay 0.3
   ;; タイプして再描画するまでの時間
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数
   anything-candidate-number-limit 100
   ;; 候補が多い時に体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
	;; root権限でアクションを実行するときのコマンド
	;; デフォルトは"su"
	(setq anything-su-or-sudo "sudo"))

  (when 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
			 (require 'migemo nil t))
	(require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
	;; lispシンボルの補完候補の再検索時間
	(anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
	(require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
	;; describe-bindingsをAnythingに置き換える
	(descbinds-anything-install)))

;; "M-y" にanything-show-kill-ringを割り当てる
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

;; anything-c-moccurの設定
(when (require 'anything-c-moccur nil t)
  (setq
   ;; anything-c-moccur用 'anything-idle-delay'
   anything-c-moccur-idle-delay 0.1
   ;; バッファの情報をハイライトする
   lanything-c-moccur-highlight-info-line-flag t
   ;; 現在選択中の候補の位置を他のwindowに表示する
   anything-c-moccur-enable-auto-look-flag t
   ;; 起動時にポイントの位置の単語を初期パターンにする
   anything-c-moccur-enable-install-pattern t)
  ;; C-M-oにanything-c-moccur-occur-by-moccurを割り当てる
  (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur))



;;; Auto Complete Mode ---------------------------------------------------
;; auto-complete の設定
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;; C-n/C-p で候補を選択
(define-key ac-complete-mode-map "¥C-n" 'ac-next)
(define-key ac-complete-mode-map "¥C-p" 'ac-previous)



;;;  ---------------------------------------------------












   
