;;; keymap
;; (defvar tera-mode-map
;;   (let ((map (make-keymap)))
;;     (define-key-map "\C-j" 'newline-and-indent)
;;     map)
;;   "Keymap for Tera major mode")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tera\\'" . tera-mode))

;;;
;;; sintax highlight
;;;
;; level 1
(defvar expr-delimiters
  '("{%" "%}" "{{" "}}" "{%-}" "-%}"))

(defvar comment-delimiters
  '("{#" "#}"))

(defvar string-concat
  '("~"))

(defvar in-checking
  '("in" "not"))

(defvar logic-keywords
  '("and" "or" "not"))

(defvar tests-keywords
  '("is" "not"))

(defvar loop-keywords
  '("loop." "index" "index0" "first" "last"))

(defvar construct-keywords
  '("raw" "endraw" "set"  "set_global" "filter" "endfilter" "block" "endblock" "if" "endif" "elif" "else" "for" "endfor" "break" "continue" "include" "macro" "endmacro" "import" "extends"))

(defvar builtin-filters-keywords
  '("lower" "wordcount" "capitalize" "replace" "addshashes" "slugify" "title" "trim" "trim_start" "trim_end" "trim_start_matches" "trim_end_matches" "truncate" "striptags" "first" "last" "nth" "join" "length" "reverse" "sort" "unique" "slice" "group_by" "filter" "map" "concat" "urlencode" "urlencode_strict" "pluralize" "round" "filesizeformat" "date" "escape" "escape_xml" "safe" "get" "split" "int" "float" "json_encode" "as_str" "default"))

(defvar builtin-tests-keywords
  '("defined" "undefined" "odd" "even" "string" "number" "divisibleby" "iterable" "object" "starting_with" "ending_with" "containing" "matching"))

(defvar builtin-functions-keywords
  '("range" "now" "throw" "get_random" "get_env"))

;;; default font lock
(defvar tera-font-lock-keywords
  `((
     ( ,(regexp-opt expr-delimiters 'words) . font-lock-comment-face)
     ( ,(regexp-opt comment-delimiters 'words) . font-lock-comment-delimiter-face)
     ( ,(regexp-opt string-concat 'words) . font-lock-keyword-face)
     ( ,(regexp-opt in-checking 'words) . font-lock-keyword-face)
     ( ,(regexp-opt logic-keywords 'words) . font-lock-keyword-face)
     ( ,(regexp-opt tests-keywords 'words) . font-lock-keyword-face)
     ( ,(regexp-opt loop-keywords 'words) . font-lock-variable-name-face)
     ( ,(regexp-opt construct-keywords 'words) . font-lock-keyword-face)
     ( ,(regexp-opt builtin-filters-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt builtin-tests-keywords 'words) . font-lock-builtin-face)
     ( ,(regexp-opt builtin-functions-keywords 'words) . font-lock-builtin-face)
     ( ,(rx (or "{%" "%}" "{%-" "-%}")) (0 font-lock-function-name-face t))
     ( ,(rx (or "{{" "}}")) (0 font-lock-type-face t))
     ( ,(rx "{#" (* whitespace) (group (*? anything)) (* whitespace) "#}") (1 font-lock-comment-face t))
     ( ,(rx (or "{#" "#}")) (0 font-lock-comment-delimiter-face t))
     ( ,(rx "{{" (* whitespace) (group (*? anything))
            (* "|" (* whitespace) (*? anything)) (* whitespace) "}}") (1 font-lock-variable-name-face t))
     ( ,(rx  (group "|" (* whitespace)) (group (+ word))) (1 font-lock-keyword-face t) (2 font-lock-warning-face t))
     ( ,(rx-to-string `(and (group "|" (* whitespace)) (group ,(append '(or) builtin-functions-keywords)))) (1 font-lock-keyword-face t) (2 font-lock-function-name-face t))
     ( ,(rx-to-string `(and word-start ,(append '(or) construct-keywords) word-end)) (0 font-lock-builtin-face))
     )))

;;;
;;; Entry function
;;;
;; the entry function can be simplified by deriving tera-mode from html-mode
(define-derived-mode tera-mode html-mode "Tera"
  "Major mode for editing Tera Template Language files."
  ;;;
  ;;; Default font lock
  ;;(set (make-local-variable 'font-lock-defaults) '(tera-font-lock-keywords))
  (setq font-lock-defaults tera-font-lock-keywords)
  ;;;
  ;;; Indentation
  ;;(set (make-local-variable 'indent-line-function) 'tera-indent-line)
  ;;;
  ;;; Syntax table
  ;;(set-syntax-table tera-mode-syntax-table)
  ;; (modify-syntax-entry ?{ "( 12" tera-mode-syntax-table)   ;; open parenthesis {{
  ;; (modify-syntax-entry ?} "( 34" tera-mode-syntax-table)   ;; close parenthesis }}
  ;; (modify-syntax-entry ?% "( 23b" tera-mode-syntax-table)  ;; close parenthesis {% %}
  ;; (modify-syntax-entry ?# "! 23b" tera-mode-syntax-table)  ;; comment delimiter {# #}
  ;; (modify-syntax-entry ?~ "_" tera-mode-syntax-table)      ;; string concatenation
  ;; (modify-syntax-entry ?- "_" tera-mode-syntax-table)      ;; remove whitespaces {%- -%}
  (setq comment-start "{#")
  (setq comment-end "#}")
  )

;;;
;;; Provide the mode
;;;
(provide 'tera-mode)
