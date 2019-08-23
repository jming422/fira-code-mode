;;; fira-code.el --- Fira Code ligatures using prettify-symbols.

;; This file is not part of GNU Emacs.

;;; License:

;; fira-code.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; fira-code.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with fira-code.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A simple package for Fira Code ligatures, built from these instructions:
;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-prettify-symbols
;; 
;; Has some of my customizations baked into it still.
;; 
;; Requires installing the Fira Code Symbol font from here:
;; https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632

;;; Code:
(defun fira-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (delete nil
     (mapcar
      (lambda (s)
	(setq idx (1+ idx))
	(when s
	  (let* ((code (+ #Xe100 idx))
		 (width (string-width s))
		 (prefix ())
		 (suffix '(?\s (Br . Br)))
		 (n 1))
	    (while (< n width)
	      (setq prefix (append prefix '(?\s (Br . Bl))))
	      (setq n (1+ n)))
	    (cons s (append prefix suffix (list (decode-char 'ucs code)))))))
      list))))

(defconst fira-code-mode--ligatures
  '(nil ;;"www"
    "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-"
    nil ;;"[]"
    "::" ":::" ":=" "!!" "!=" "!==" "-}"
    "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
    nil ;;"#{"
    "#[" "##" "###" "####"
    nil ;;"#("
    "#?"
    nil nil ;;"#_" "#_("
    ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
    "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
    "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
    "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
    ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
    "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
    "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
    "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
    "x" ":" "+" "+" "*"))

(defvar fira-code-mode--old-prettify-alist)

(defun fira-code-mode--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
  (prettify-symbols-mode t))

(defun fira-code-mode--disable ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter " Fira"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-mode
      (fira-code-mode--enable)
    (fira-code-mode--disable)))

(defun fira-code-mode--setup ()
  "Setup Fira Code Symbols."
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

(provide 'fira-code-mode)
;;; fira-code.el ends here
