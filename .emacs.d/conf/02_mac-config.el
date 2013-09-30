;; Macだけに読み込ませる内容
(when (eq system-type 'darwin)
  ;; MacのEmacsでファイル名を正しく扱うための設定
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)

  ;; 環境変数の設定
  (add-to-list 'exec-path "/opt/local/bin")
  (add-to-list 'exec-path "/usr/local/bin")

  ;;; フォント設定
  ;; asciiフォントをMenloに
  (set-face-attribute 'default nil
					  :family "Menlo"
					  :height 120)
  ;; 日本語フォントをヒラギノ明朝 Proに
  (set-fontset-font
   nil 'japanese-jisx0208
   (font-spec :family "Hiragino Kaku Gothic ProN"))

  ;; フォントの横幅を調整する
  (setq face-font-rescale-alist
		'((".*Menlo.*" . 1.0)
		  (".*Hiragino_Mincho_Pro.*" . 1.2)
		  ("-cdac$" . 1.3)))
)


;; Windowsだけに読み込ませる内容
(when (eq system-type 'w32)
  ;;ファイル名の設定
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932)

  ;;; フォント設定
  ;; asciiフォントをConsolasに
  (set-face-attribute 'default nil
					  :family "Consolas"
					  :height 120)
  ;; 日本語フォントをメイリオに
  (set-fontset-font
   nil 'japanese-jisx0208
   (font-spec :family "Meiryo"))

)
