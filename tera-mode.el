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

;;; default level
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
     )))

;;;
;;; Indentation
;;;
;; (defun tera-indent-line ()
;;   "Indent current line as Tera code"
;;   (interactive)  ;; allows to call the function directly with M-x
;;   (beginning-of-line) ;; set the point to the beginning of the line
;;   ;; check if is the first line in the buffer
;;   (if (bobp)
;;       (indent-line-to 0))
;;   )

;;;
;;; Syntax table
;;;
(defvar tera-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?{ "( 12" st)  ;; open parenthesis {{
    (modify-syntax-entry ?} "( 34" st)  ;; close parenthesis }}
    (modify-syntax-entry ?% "( 23b" st)  ;; close parenthesis {% %}
    (modify-syntax-entry ?# "! 23b" st)  ;; comment delimiter {# #}
    (modify-syntax-entry ?~ "_" st)  ;; string concatenation
    (modify-syntax-entry ?- "_" st)  ;; remove whitespaces {%- -%}
    st)
  "Syntax table for tera-mode")

;;;
;;; Entry function
;;;
;; the entry function can be simplified by deriving tera-mode from html-mode
(define-derived-mode tera-mode html-mode "Tera"
  "Major mode for editing Tera Template Language files."
  ;; (set (make-local-variable 'font-lock-defaults) '(tera-font-lock-keywords))
  (setq font-lock-defaults tera-font-lock-keywords)
  ;; (set (make-local-variable 'indent-line-function) 'tera-indent-line)
  (setq comment-start "{#")
  (setq comment-end "#}")
  (set-syntax-table tera-mode-syntax-table)
  )

;;;
;;; Provide the mode
;;;
(provide 'tera-mode)
