;;; connect.el is a an emacs package which should connect the org-assistent js backend to emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Daniel Rosel
;;
;; Author: Daniel Rosel <daniel@alves.world>
;; Maintainer: Daniel Rosel <daniel@alves.world>
;; Created: June 20, 2022
;; Modified: June 20, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/velo/connect
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(setq path-to-backend "~/Documents/Projects/org-assistant/v1/demo.js")

(defun connect-create-timeline (events)
  (shell-command (concat "node " path-to-backend " \"" (format "%s" events) "\"")))


(setq events '(
               ("This" "%" "60")
               ("That" "14:30" "50")
               ("Work" "10:00" "70")
               ))

(connect-create-timeline events)

(provide 'connect)
;;; connect.el ends here
