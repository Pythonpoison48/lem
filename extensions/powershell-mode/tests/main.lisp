(defpackage :lem-powershell-mode/tests
  (:use :cl :rove :lem :lem-powershell-mode)
  (:import-from :lem/language-mode
                :language-mode
                :line-comment
                :insertion-line-comment))
(in-package :lem-powershell-mode/tests)

(defun make-powershell-buffer (text)
  (let ((buffer (make-buffer "*powershell-test*" :temporary t :enable-undo-p nil)))
    (change-buffer-mode buffer 'powershell-mode)
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    buffer))

(defun reindent (&rest lines)
  (with-current-buffers ()
    (let ((buffer (make-powershell-buffer
                   (format nil "~{~A~^~%~}"
                           (mapcar (lambda (line) (string-left-trim " " line)) lines)))))
      (setf (current-buffer) buffer)
      (indent-buffer buffer)
      (points-to-string (buffer-start-point buffer)
                        (buffer-end-point buffer)))))

(defun lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(deftest mode-registration
  (ok (find-mode "PowerShell"))
  (ok (subtypep 'powershell-mode 'language-mode)))

(deftest comment-variables
  (with-current-buffers ()
    (let ((buffer (make-powershell-buffer "")))
      (ok (equal "#" (variable-value 'line-comment :buffer buffer)))
      (ok (equal "# " (variable-value 'insertion-line-comment :buffer buffer))))))

(deftest indent-blocks
  (let ((expected (list "function Foo {"
                        "    if ($x) {"
                        "        Write-Host $x"
                        "    }"
                        "}")))
    (ok (equal (apply #'lines expected)
               (apply #'reindent expected)))))

(deftest indent-pipeline-continuation
  (let ((expected (list "Get-ChildItem |"
                        "    Where-Object { $_ } |"
                        "    Sort-Object")))
    (ok (equal (apply #'lines expected)
               (apply #'reindent expected)))))

(deftest indent-backtick-continuation
  (let ((expected (list "Get-ChildItem -Path . `"
                        "    -Recurse")))
    (ok (equal (apply #'lines expected)
               (apply #'reindent expected)))))
