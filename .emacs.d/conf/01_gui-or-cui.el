;; ターミナル以外はツールバー、スクロールバーを非表示
(when window-system
  ;; tool-bar を表示
  (tool-bar-mode 0)
  ;; scroll-barを非表示
  (scroll-bar-mode 9)

  ;; ウィンドウを透明化
  ;; (add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))
)


;; Cocoa Emacs以外はメニューバーを非表示
(unless (eq window-system 'ns)
  ;; menu-barを非表示
  (menu-bar-mode 0)

)
