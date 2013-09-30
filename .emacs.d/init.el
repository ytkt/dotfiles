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
(add-to-load-path "elisp" "conf" "public_repos" "elpa")

;; ELPAリポジトリの追加
(require 'package)
(add-to-list 'package-archives
			 '("marmalade" . "http://marmalade-repo.org/packages/")
			 '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; init-loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf")

;; スタートアップページを表示しない
(setq inhibit-startup-message t)

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
(setq linum-format "%4d ")


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
;; parenのスタイル: exressionは括弧内も強調表示
;;(setq show-paren-style 'exression)
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



;;; color-moccurの設定 ---------------------------------------------------
(when (require 'color-moccur nil t)
  ;; M-oにoccur-by-moccurを割り当て
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索のときに除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "¥¥.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "¥#.+#$")
  ;; Migemoで利用できる環境であればMigemoを使う
  (when (and (executable-find "cmigemo")
			 (require 'migemo nil t))
	(setq moccur-use-migemo t)))

;; moccur-editの設定
(require 'moccur-edit nil t)
;; moccur-edit-finish-editと同時にファイルを保存する
(defadvice moccur-edit-change-file
  (after save-after-moccur-edit-buffer activate)
  (save-buffer))


;; grepの結果を直接編集
(require 'wgrep nil t)


;;; 履歴管理 ---------------------------------------------------
;; undohist
(when (require 'undohist nil t)
  (undohist-initialize))




;;; ウィンドウ管理 ---------------------------------------------------
;;ElScreen




;;; 各種言語の開発環境 ---------------------------------------------------
;; nxml-mode
;; (add-to-list 'auto-mode-alist '("¥¥.[sx]?html?¥¥(||.[a-zA-Z]+¥¥?¥¥'" . nxml-mode)
;; HTML5
;;(eval-after-load "rng-loc"
;;  '(add-to-list 'rng-schema-locating-files
;;				"~/.emacs.d/public_repos/html5-el/schemas.xml"))
;;(require 'whattf-dt)
;; </を入力すると自動的にタグを閉じる
;;(setq nxml-slash-auto-complete-flag t)
;; M-TABでタグを補完する
;;(setq nxml-bind-meta-tab-to-complete-flag t)
;; nxml-modeでauto-complete-modeを利用する
;;(add-to-list 'ac-modes 'nxml-mode)
;; 子要素のインデント幅を設定する
;;(setq nxml-child-indent 0)
;; 属性値のインデント幅を設定する
;;(setq nxml-attribute-indent 0)



;; cssm-mode
(defun css-mode-hooks ()
  "css-mode hooks"
  ;; インデントをCスタイルにする
  (setq css-indent-function #'cssm-c-style-indenter)
  ;; インデント幅を2にする
  (setq cssm-indent-level 2)
  ;; インデントにタブ文字を使わない
  (setq-default indent-tabs-mode nil)
  ;; 閉じカッコの前に改行を挿入する
  (setq cssm-newline-before-closing-bracket t))
(add-hook 'css-mode-hook 'css-mode-hooks)

;; javascript
;; js-mode
(defun js-indent-hook()
  ;; インデント幅を4にする
  (setq js-indent-level 4
		js-expr-indent-offset 4)
;		indent-tabs-mode nil)
  ;; switch文のcaseラベルをインデントする関数を定義する
;  (defun my-js-indent-line ()
;	(interactive)
;	(let*  ((parse-status (save-excursion (syntax-ppss (point-to-bol))))
;			(offset (- (current-column) (current-indentation)))
;			(indentation (js--proper-indentation parse-status)))
;	  (back-to-indentation)
;	  (if (looking-at "case¥¥s-")
;		  (indent-line-to (+ indentation 2))
;		(js-indent-line))
;	  (when (> offset 0) (forward-char offset))))
  ;; caseラベルのインデント処理をセットする
;  (set (make-local-variable 'indent-line-function) );'my-js-indent-line)
  ;; ここまでcaseラベルを調整する設定
  )
;; js-modeの起動時にhookを追加
(add-hook 'js-mode-hook 'js-indent-hook)

;; php-mode
(when (require 'php-mode nil t)
  (add-to-list 'auto-mode-alist '("¥¥.ctp¥¥'" . php-mode))
  (setq php-search-url "http://jp.php.net/ja")
  (setq php-manual-url "http://jp/php/net/manual/ja/"))
;; C-c C-fで関数のリファレンスを調べられる
;; php-modeのインデント設定
(defun php-indent-hook ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4) ; インデントの基本の文字数
  ;; (c-set-offset 'case-label '+)  ; switch文のcaseラベル
  (c-set-offset 'arglist-intro '+)  ; 配列の最初の要素が改行した場合
  (c-set-offset 'arglist-close 0))  ; 配列の閉じ括弧
(add-hook 'php-mod1e-hook 'php-indent-hook)

;; php-completion
(defun php-completion-hook ()
  (when (require 'php-completion nil t)
	(php-completion-mode t)
	(define php-mode-map (kbd "C-o") 'phpcmp-complete)
	(when (require 'auto-complete nil t)
	  (make-variable-buffer-local 'ac-sources)
	  (add-to-list 'ac-sources 'ac-sources-php-completion)
	  (auto-complete-mode t))))
(add-hook 'php-mode-hook 'php-completion-hook)

;; perl-modeをcperl-modeのエイリアスにする
(defalias 'perl-mode 'cperl-mode)

;; cperl-modeにインデント設定
(setq cperl-indent-level 4           ; インデント幅を4へ
	  cperl-continued-brace-offset 4 ; 継続する文のオフセット
	  cperl-brace-offset -4          ; ブレースのオフセット
	  cperl-label-offset -4          ; labelのオフセット
	  cperl-indent-parens-as-block t ; 括弧もブロックとしてインデント
	  cperl-close-paren-offset -4    ; 閉じ括弧のオフセット
	  cperl-tab-always-indent t      ; タブをインデントにする
	  cpeap-highlight-variables-indiscriminately t) ;スカラを常にハイライト

;; yaml-mode
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("¥¥.yml$" . yaml-mode)))

;; perl-completion
(defun perl-completion-hook ()
  (when (require 'perl-completion nil t)
	(perl-completion-mode t)
	(when (require 'auto-complete nil t)
	  (auto-complete-mode t)
	  (make-variable-buffer-local 'ac-sources)
	  (setq ac-sources
			'(ac-source-perl-completion)))))
(add-hook 'cperl-mode-hook 'perl-completion-hook)

;; ruby-mode
;; 括弧の自動挿入
(require 'ruby-electric nil t)
;; endに対応する行のハイライト
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))
;; インタラクティブrubyを利用する
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
;; ruby-mode-hook用の関すを定義
(defun ruby-mode-hooks ()
  (inf-ruby-keys)
  (ruby-electric-mode t)
  (ruby-block-mode t))
;; ruby-mode-hookに追加
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)


;;; aliases ---------------------------------------------------
;; dtwをdelete-trailing-whitespaceのエイリアスにする
(defalias 'dtw 'delete-trailing-whitespace)



;;; Flymake ---------------------------------------------------
;; C系言語でMakefileが無い場合
;; Makefileの種類を定義
;;(defvar flymake-makefile-filenames
;;  '("Makefile" "makefile" "GNUmakefile")
;;  "File name for make.")
;; Makefileがなければコマンドを直接利用するコマンドラインを生成
;(defun flymake-get-make-gcc-cmdline (source base-dir)
;  (let (found)
;	(dolist (makefile flymake-makefile-filenames)







;;; Rinari ---------------------------------------------------
(require 'ido)
(ido-mode t)
(require 'rinari)
;; yasnippet
(require 'yasnippet)
;(yas/initialize)
;(yas/load-directory "~/.emacs.d/elisp/yasnippet/snippets")
;(yas/load-directory "~/.emacs.d/elisp/yasnippet-rails/rails-snippets")
 


;;;  ---------------------------------------------------
;; ビープ音を消す
(setq ring-bell-function 'ignore)

;;;  ---------------------------------------------------


;;;  ---------------------------------------------------








