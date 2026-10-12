(defpackage :lem-powershell-mode
  (:use :cl :lem :lem/language-mode)
  (:export :*powershell-mode-hook*
   :*powershell-mode-keymap*
           :*powershell-syntax-table*
   :powershell-mode))
(in-package :lem-powershell-mode)

(defvar *powershell-keywords*
  '("begin" "break" "catch" "class" "clean" "configuration" "continue" "data"
    "define" "do" "dynamicparam" "else" "elseif" "end" "enum" "exit" "filter"
    "finally" "for" "foreach" "from" "function" "hidden" "if" "in"
    "inlinescript" "parallel" "param" "process" "return" "sequence" "static"
    "switch" "throw" "trap" "try" "until" "using" "var" "while" "workflow"))

(defvar *powershell-comparison-operators*
  '("eq" "ne" "gt" "ge" "lt" "le" "like" "notlike" "match" "notmatch"
    "contains" "notcontains" "in" "notin" "replace" "split"))

(defvar *powershell-operators*
  '("and" "or" "xor" "not" "band" "bor" "bxor" "bnot" "shl" "shr"
    "join" "is" "isnot" "as" "f"))

(defvar *powershell-constants*
  '("true" "false" "null"))

(defvar *powershell-defun-regex*
  "(?i)^\\s*(?:function|filter|workflow|configuration|class|enum)\\s+")

(defvar *powershell-continuation-regex*
  "(?:`|\\|)\\s*$")

(defvar *powershell-syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\_ #\-)
   :paren-pairs '((#\( . #\))
                  (#\{ . #\})
                  (#\[ . #\]))
   :string-quote-chars '(#\" #\')
   :escape-chars '(#\`)
   :line-comment-string "#"
   :block-comment-pairs '(("<#" . "#>"))
   :block-string-pairs '(("@\"" . "\"@")
                         ("@'" . "'@"))))



(defun beginning-of-defun (point n)
  (loop :repeat n
        :do (search-backward-regexp point *powershell-defun-regex*)))

(defun end-of-defun (point n)
  (if (minusp n)
      (beginning-of-defun point (- n))
      (loop :repeat n
            :do (search-forward-regexp point "^\\}"))))

(defun enclosing-block-column (point)
  (with-point ((p point))
    (when (scan-lists p -1 1 t)
      (back-to-indentation p)
      (point-column p))))

(defun previous-non-blank-line (point)
  (with-point ((p point))
    (loop
      (unless (line-offset p -1)
        (return nil))
      (unless (blank-line-p p)
        (return (line-string p))))))

(defun continuation-line-p (point)
  (alexandria:when-let ((line (previous-non-blank-line point)))
    (ppcre:scan *powershell-continuation-regex* line)))

(defun calc-indent (point)
  (let ((tab-width (variable-value 'tab-width :default point)))
    (with-point ((p point))
      (back-to-indentation p)
      (cond ((or (in-string-p p)
                 (in-comment-p p))
             nil)
            ((member (character-at p) '(#\} #\] #\)))
             (or (enclosing-block-column p) 0))
            (t
             (+ (alexandria:if-let ((column (enclosing-block-column p)))
                  (+ column tab-width)
                  0)
                (if (continuation-line-p p) tab-width 0)))))))

(defun alternation (strings)
  (list* :alternation (sort (copy-list strings) #'> :key #'length)))

(defun word-tokens (strings)
  (list :sequence
        :case-insensitive-p
        '(:negative-lookbehind (:char-class :word-char-class #\-))
        (alternation strings)
        '(:negative-lookahead (:char-class :word-char-class #\-))))

(defun operator-tokens ()
  (list :sequence
        :case-insensitive-p
        '(:negative-lookbehind (:char-class :word-char-class #\-))
        #\-
        (list :alternation
              (list :sequence
                    '(:greedy-repetition 0 1 (:char-class #\c #\i))
                    (alternation *powershell-comparison-operators*))
              (alternation *powershell-operators*))
        '(:negative-lookahead (:char-class :word-char-class #\-))))

(defun constant-tokens ()
  (list :sequence
        :case-insensitive-p
        #\$
        (alternation *powershell-constants*)
        '(:negative-lookahead (:char-class :word-char-class))))

(defun make-variable-patterns ()
  (make-tm-patterns
   (make-tm-match (constant-tokens)
                  :name 'syntax-constant-attribute)
   (make-tm-match "(?i)\\$(?!(?:true|false|null)(?!\\w))(?:\\{[^}]*\\}|[$?^]|(?:[a-z]+:)?[a-z_][a-z0-9_]*)"
                  :name 'syntax-variable-attribute)))

(defun make-tmlanguage-powershell ()
  (let ((patterns
          (make-tm-patterns
           (make-tm-region '(:sequence "<#")
                           '(:sequence "#>")
                           :name 'syntax-comment-attribute)
           (make-tm-region "#" "$" :name 'syntax-comment-attribute)
           (make-tm-region "@\"\\s*$" "^\"@" :name 'syntax-string-attribute)
           (make-tm-region "@'\\s*$" "^'@" :name 'syntax-string-attribute)
           (make-tm-region '(:sequence "\"")
                           '(:sequence "\"")
                           :name 'syntax-string-attribute
                           :patterns (make-tm-patterns
                                      (make-tm-match "`." :name 'syntax-string-attribute)
                                      (make-tm-match "\"\"" :name 'syntax-string-attribute)
                                      (make-tm-include "#variables")))
           (make-tm-region '(:sequence "'")
                           '(:sequence "'")
                           :name 'syntax-string-attribute
                           :patterns (make-tm-patterns
                                      (make-tm-match "''" :name 'syntax-string-attribute)))
           (make-tm-match "(?i)(?<![\\w-])(function|filter|workflow|configuration)\\s+((?:[a-z_]+:)?[\\w-]+)"
                          :captures (vector nil
                                            (make-tm-name 'syntax-keyword-attribute)
                                            (make-tm-name 'syntax-function-name-attribute)))
           (make-tm-match "(?i)(?<![\\w-])(class|enum)\\s+([a-z_]\\w*)"
                          :captures (vector nil
                                            (make-tm-name 'syntax-keyword-attribute)
                                            (make-tm-name 'syntax-type-attribute)))
           (make-tm-match "\\[([A-Za-z_][\\w.]*)(?=\\s*[\\]\\[(])"
                          :captures (vector nil
                                            (make-tm-name 'syntax-type-attribute)))
           (make-tm-include "#variables")
           (make-tm-match "(?i)@[a-z_][a-z0-9_]*"
                          :name 'syntax-variable-attribute)
           (make-tm-match (word-tokens *powershell-keywords*)
                          :name 'syntax-keyword-attribute)
           (make-tm-match (operator-tokens)
                          :name 'syntax-builtin-attribute)
           (make-tm-match "(?i)\\b(?:0x[0-9a-f]+|[0-9]+(?:\\.[0-9]+)?(?:e[+-]?[0-9]+)?)(?:kb|mb|gb|tb|pb|[ld])?\\b"
                          :name 'syntax-constant-attribute)))
        (repository (make-tm-repository)))
    (add-tm-repository repository "variables" (make-variable-patterns))
    (make-tmlanguage :patterns patterns :repository repository)))

(set-syntax-parser *powershell-syntax-table* (make-tmlanguage-powershell))



(defun tree-sitter-query-path ()
  "Return the path to the tree-sitter highlight query for Powershell."
  (asdf:system-relative-pathname :lem-powershell-mode "tree-sitter/highlights.scm"))


(defun try-enable-tree-sitter ()
  "Try to enable tree-sitter for Powershell mode. Falls back to tmlanguage if unavailable."
  (ignore-errors
   (when (and (find-package :lem-tree-sitter)
              (fboundp (find-symbol "TREE-SITTER-AVAILABLE-P" :lem-tree-sitter))
              (funcall (find-symbol "TREE-SITTER-AVAILABLE-P" :lem-tree-sitter)))
     (funcall (find-symbol "ENABLE-TREE-SITTER-FOR-MODE" :lem-tree-sitter)
              *powershell-syntax-table* "Powershell" (tree-sitter-query-path)
              :indent-query-path (tree-sitter-indent-query-path))
     t)))


(define-major-mode powershell-mode language-mode
  (:name "PowerShell"
   :description "Major mode for editing PowerShell scripts."
   :keymap *powershell-mode-keymap*
   :syntax-table *powershell-syntax-table*
   :mode-hook *powershell-mode-hook*)
  (try-enable-tree-sitter)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 4
        (variable-value 'calc-indent-function) 'calc-indent
        (variable-value 'beginning-of-defun-function) 'beginning-of-defun
        (variable-value 'end-of-defun-function) 'end-of-defun
        (variable-value 'line-comment) "#"
        (variable-value 'insertion-line-comment) "# "))

(define-file-type ("ps1" "psm1" "psd1") powershell-mode)
(define-program-name-with-mode ("pwsh" "powershell") powershell-mode)
