(defvar tera-mode-hook nil)

;;; keymap
;; (defvar tera-mode-map
;;   (let ((map (make-keymap)))
;;     (define-key-map "\C-j" 'newline-and-indent)
;;     map)
;;   "Keymap for Tera major mode")

;;; autoload
(add-to-list 'auto-mode-alist '("\\.tera\\'" . tera-mode))

;;;
;;; sintax highlight
;;;
;; level 1
(defconst tera-font-lock-keywords-delimiters
  (list
   '(((regexp-opt '("{%" "%}" "{{" "}}" "{%-}" "-%}")) t) . font-lock-comment-face))
  "Minimal highlighting expressions for Tera mode")

;; level 2
(defconst tera-font-lock-keywords-comments
  (append tera-font-lock-keywords-delimiters (list '(((regexp-opt '("{#" "#}")) t) . font-lock-comment-delimiter-face)))
  "Comments highlighting in Tera mode")

;; level 3
(defconst tera-font-lock-keywords-string-concatenation
  (append tera-font-lock-keywords-comments (list '(((regexp-opt '("~")) t) . font-lock-keyword-face)))
  "String concatenation highlighting in Tera mode")

;; level 4
(defconst tera-font-lock-keywords-in-checking
  (append tera-font-lock-keywords-string-concatenation (list '(((regexp-opt '("in" "not") t) . font-lock-keyword-face))))
  "In checking highlighting in Tera mode")

;; level 5
(defconst tera-font-lock-keywords-logic
  (append tera-font-lock-keywords-in-checking (list '(((regexp-opt '("and" "or" "not") t) . font-lock-variable-name-face))))
  "Logic highlighting in Tera mode")

;; level 6
(defconst tera-font-lock-keywords-tests
  (append tera-font-lock-keywords-logic (list '((regexp-opt '("is" "not") t) . font-lock-variable-name-face)))
  "Tests highlighting in Tera mode")

;; level 7
(defconst tera-font-lock-keywords-loop-variables
  (append tera-font-lock-keywords-tests (list '(((regexp-opt '("loop." "index" "index0" "first" "last") t) . font-lock-variable-name-face))))
  "Loop variables highlighting in Tera mode")

;; level 8
(defconst tera-font-lock-keywords-constructs
  (append tera-font-lock-keywords-loop-variables (list '(((regexp-opt '("raw" "endraw" "set"  "set_global" "filter" "endfilter" "block" "endblock" "if" "endif" "elif" "else" "for" "endfor" "break" "continue" "include" "macro" "endmacro" "import" "extends") t) . font-lock-keyword-face))))
  "Constructs highlighting in Tera mode")

;; level 9
(defconst tera-font-lock-keywords-builtin-filters
  (append tera-font-lock-keywords-constructs (list '(((regexp-opt '("lower" "wordcount" "capitalize" "replace" "addshashes" "slugify" "title" "trim" "trim_start" "trim_end" "trim_start_matches" "trim_end_matches" "truncate" "striptags" "first" "last" "nth" "join" "length" "reverse" "sort" "unique" "slice" "group_by" "filter" "map" "concat" "urlencode" "urlencode_strict" "pluralize" "round" "filesizeformat" "date" "escape" "escape_xml" "safe" "get" "split" "int" "float" "json_encode" "as_str" "default") t) . font-lock-builtin-face))))
  "Builtin filters highlighting in Tera mode")

;; level 10
(defconst tera-font-lock-keywords-builtin-tests
  (append tera-font-lock-keywords-builtin-filters (list '(((regexp-opt '("defined" "undefined" "odd" "even" "string" "number" "divisibleby" "iterable" "object" "starting_with" "ending_with" "containing" "matching")) t) . font-lock-builtin-face)))
  "Builtin tests highlighting in Tera mode")

;; level 11
(defconst tera-font-lock-keywords-builtin-functions
  (append tera-font-lock-keywords-builtin-tests (list '(((regexp-opt '("range" "now" "throw" "get_random" "get_env")) t) . font-lock-builtin-face)))
  "Builtin functions highlighting in Tera mode")

;;; define the default level of highlighting to be maximum
(defvar tera-font-lock-keywords tera-font-lock-keywords-builtin-functions
  "Default highlighting expressions for Tera mode")
;;; ---------------------

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
(defun tera-mode()
  "Major mode for editing Tera Template Language files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table tera-mode-syntax-table)
  ;;(use-local-map tera-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(tera-font-lock-keywords))
  ;;(set (make-local-variable 'indent-line-function) 'tera-indent-line)
  (setq major-mode 'tera-mode)
  (setq mode-name "Tera")
  (run-hooks 'tera-mode-hook)
  )

;; the entry function can be simplified by deriving tera-mode from html-mode
;; (define-derived-mode tera-mode html-mode "Tera"
;;   "Major mode for editing Tera Template Language files."
;;   (set (make-local-variable 'font-lock-defaults) '(tera-font-lock-keywords))
;;   (set (make-local-variable 'indent-line-function) 'tera-indent-line)
;;   )

;;;
;;; Provide the mode
;;;
(provide 'tera-mode)
