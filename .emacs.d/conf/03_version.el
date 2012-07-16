;; versionの違いによる違いを吸収する

;(global-set-key "¥M-n" 'linum-mode)


;; Emacs 23より前のバージョンを利用している
(when (> emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))



;; TODO ロードパスの読み込み設定の後に移動
;; Emacs 24以前のバージョンを利用している
(when (> emacs-major-version 24)
  ;; package.elの設定
  (when (require 'package nil t)
	(add-to-list 'package-archives
				 '("marmalade" . "http://marmalade-repo.prg/packages/"))
	(add-to-list 'package-archives
				 '("ELPA" . "http://tromey.com/elpa/"))
	(package-initialize))
)

