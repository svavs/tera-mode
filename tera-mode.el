;;; NOTES:
;;;   - two syntax matching types:
;;;     1) "regex" . font-lock-name
;;;     2) "regex" (capture-group-id font-lock-name)
;;;   - variable name regex: \\(?:\\w\\|\\.\\|_\\)+
;;;
;;; TODO:
;;;   - strings can be in single quotes
;;;   - detect string vs. object value types
;;;   - detect boolean operators (and|or)
;;;   - add groups for cycle tags
;;;   - for: 
;;;     - add limit:n, offset:n
;;;     - add reversed
;;;     - add support for ranges


;;;;; Tera template language
;;
;; string concatenation : {{ a ~ b }}
;; in checking : {{ a in b }} | {{ a not in b }}
;; filter sections : {% filter filtername %} a {% endfilter %}
;; tests : {% if number is|is not odd %} odd {% endif %}
;; control structures :
;;; {% if price < 10 or always_show %} Price is {{ price }}.{% elif price > 1000 and not rich %}That's expensive!{% else %} N/A {% endif %}
;;; {% for product in products %} {{loop.index}}. {{product.name}} {% endfor %}
;;; loop controls : {% break %} , {% continue %}
;;; for loop variables : loop.index|loop.index0|loop.first|loop.last
;; include : {% include "included.html" %}
;; macros :
;;; {% macro input(label, type="text") %} <label> {{ label }} <input type="{{type}}" /> </label> {% endmacro input %}
;;; {% import "macros.html" as macros %}
;;; // namespace::macro_name(**kwargs) {{ macros|self::input(label="Name", type="text") }}
;; inheritance :
;;; base.html : {% block head %}{% endblock head %}
;;; child : {% extends "base.html" %}
;; built-ins :
;;; filters :
;;;; lower\\wordcount\\capitalize\\replace\\addslashes\\slugify\\title\\trim\\trim_start\\trim_end\\trim_start_matches\\trim_end_matches\\
;;;; truncate\\striptags\\first\\last\\nth\\join\\length\\reverse\\sort\\unique\\slice\\group_by\\filter\\map\\concat\\urlencode\\
;;;; urlencode_strict\\pluralize\\round\\filesizeformat\\date\\escape\\escape_xml\\safe\\get\\split\\int\\float\\json_encode\\as_str\\default\\
;;; tests :
;;;; defined\\|undefined\\|odd\\|even\\|string\\|number\\|divisibleby\\|iterable\\|object\\|starting_with\\|ending_with\\|containing\\|matching\\
;;; functions :
;;;; range\\|now\\|throw\\|get_random\\|get_env\\


(setq teraKeywords
    '(
        ; font-lock-function-name-face : for the name of a function being defined or declared
        ; font-lock-variable-name-face : for the name of a variable being defined or declared
        ; font-lock-keyword-face : for a keyword with special syntactic significance, like ‘for’ and ‘if’ in C.
        ; font-lock-comment-face : for comments.
        ; font-lock-comment-delimiter-face : for comments delimiters, like ‘/*’ and ‘*/’ in C. On most terminals, this inherits from font-lock-comment-face.
        ; font-lock-type-face : for the names of user-defined data types.
        ; font-lock-constant-face : for the names of constants, like ‘NULL’ in C.
        ; font-lock-builtin-face : for the names of built-in functions.
        ; font-lock-preprocessor-face : for preprocessor commands. This inherits, by default, from font-lock-builtin-face.
        ; font-lock-string-face : for string constants.
        ; font-lock-doc-face : for documentation strings in the code. This inherits, by default, from font-lock-string-face.
        ; font-lock-negation-char-face : for easily-overlooked negation characters. 


	      ;;; core stuff
        ("{%\\|%}\\|{{\\|}}|{%-\\-%}|[\\]" . font-lock-comment-face) ;;; delimiters
        ("{#\\#}" . font-lock-comment-delimiter-face) ;;; comments delimiter
        ("\\~\\" . font-lock-keyword-face) ;;; string concatenation
        ("{%\s*\\(in\\|not in\\)" (1 font-lock-keyword-face)) ;;; in checking
        ("\\and\\|\\or\\|\\not\\" . font-lock-keyword-face) ;;; logic
        ("{%\s*\\(filter\\|endfilter\\)" (1 font-lock-keyword-face)) ;;; filter sections
        ("\\is\\|\\is not\\" . font-lock-keyword-face) ;;; tests
        ("{%\s*\\(if\\|elif\\|else\\|endif\\|for\\|endfor\\|break\\|continue\\)" (1 font-lock-keyword-face)) ;;; control structures
        ("loop" . font-lock-keyword-face) ;;; loop
        ("loop.\\(index\\|index0\\|first\\|last\\)" . (1 font-lock-variable-name-face)) ;;; loop variables
        ("{%\s*\\include" (1 font-lock-variable-name-face)) ;;; include
        ("{%\s*\\(macro\\|endmacro\\)" (1 font-lock-keyword-face)) ;;; macros
        ("{%\s*\\(block\\|endblock\\|extends\\)" (1 font-lock-keyword-face)) ;;; inheritance
        ("lower\\|wordcount\\|capitalize\\|replace\\|addslashes\\|slugify\\|title\\|trim\\|trim_start\\|trim_end\\|trim_start_matches\\|trim_end_matches\\|truncate\\|striptags\\|first\\|last\\|nth\\|join\\|length\\|reverse\\|sort\\|unique\\|slice\\|group_by\\|filter\\|map\\|concat\\|urlencode\\|urlencode_strict\\|pluralize\\|round\\|filesizeformat\\|date\\|escape\\|escape_xml\\|safe\\|get\\|split\\|int\\|float\\|json_encode\\|as_str\\|default\\" . font-lock-builtin-face) ;;; builtin filters
        ("defined\\|undefined\\|odd\\|even\\|string\\|number\\|divisibleby\\|iterable\\|object\\|starting_with\\|ending_with\\|containing\\|matching\\" . font-lock-builtin-face) ;;; builtin tests
        ("range\\|now\\|throw\\|get_random\\|get_env\\" . font-lock-builtin-face) ;;; builtin functions

        ("{%\s*\\(?:assign\\|capture\\|for\\|if\\|unless\\|case\\|when\\)\s+\\(\\(?:\\w\\|\\.\\|_\\)+\\)" (1 font-lock-variable-name-face)) ;;; variable after set|for|if
        ("{{\s*\\(?:\\w\\|\\.\\)+\s+|\s+\\(\\w+\\)" (1 font-lock-type-face)) ;;; variable after set|for|if
        ("{%\s*\\(?:\\w\\|\\.\\)+\s+|\s+\\w+\s+|\s+\\(\\w+\\)" (1 font-lock-type-face)) ;;; variable after set|for|if
        ("\s+|\s+" . font-lock-comment-face) ;;; tag delimiters
        ("{{\s*\\(\\(?:\\w\\|\\.\\)+\\)" (1 font-lock-variable-name-face)) ;;; variable/object being outputted
        ("{%\s*\\(?:if\\)\s+\\(?:\\w\\|\\.\\)+\s+\\(contains\\|>\\|<\\|==\\|!=\\)" (1 font-lock-keyword-face)) ;;; operators
        ("{%\s*for\s+\\w+\s+\\(in\\)" (1 font-lock-keyword-face)) ;;; the 'in' in "for temp in collection"
        ("{%\s*for\s+\\w+\s+in\s+\\(\\(?:\\w\\|\\.\\|_\\)+\\)" (1 font-lock-variable-name-face)) ;;; the 'collection' in "for temp in collection"
	   )
)

(define-derived-mode tera-mode html-mode
  (setq font-lock-defaults '(teraKeywords))
  (setq mode-name "tera mode")
  )
(add-to-list 'auto-mode-alist '("\\.tera$" . tera-mode))
(provide 'tera-mode)
