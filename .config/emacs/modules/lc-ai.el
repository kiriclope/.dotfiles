;;  -*-lexical-binding: t-*-
;;; ai tools

;;;; --- gptel ---
(use-package gptel
  :straight '(gptel :host github
                    :repo "karthink/gptel"
                    :branch "master")

  :init
  (setq-default gptel-model "gpt-3.5-turbo"
                gptel-playback t
                gptel-api-key (lambda () (string-trim (shell-command-to-string "pass openai")))
                gptel-default-mode 'org-mode))

(provide 'lc-ai)
;;; lc-ai.el ends here
