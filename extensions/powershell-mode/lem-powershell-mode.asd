(defsystem "lem-powershell-mode"
  :depends-on ("lem/core")
  :serial t
  :components ((:file "powershell-mode"))
  :in-order-to ((test-op (test-op "lem-powershell-mode/tests"))))

(defsystem "lem-powershell-mode/tests"
  :depends-on ("lem-powershell-mode" "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
