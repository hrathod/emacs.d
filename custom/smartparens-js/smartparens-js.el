;;; smartparens-js -- better parenthetical movement for Javascript
;;; Commentary:
;; paren
(require 'smartparens)

;;; Code:
(add-to-list 'sp-sexp-suffix '(js2-mode syntax ""))
(add-to-list 'sp-navigate-consider-stringlike-sexp 'js2-mode)

(provide 'smartparens-js)
;;; smartparens-js ends here
