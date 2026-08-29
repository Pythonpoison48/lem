(defpackage :lem-tests/lsp-mode/bench
  (:use :cl)
  (:local-nicknames (:alexandria :alexandria))
  (:import-from :lem
                #:make-buffer
                #:delete-buffer
                #:buffer-point
                #:insert-string
                #:buffer-end
                #:symbol-string-at-point
                #:completion-strings)
  ;; completion-strings est reexporté par :lem (src/internal-packages.lisp:605)
  (:import-from :lem-core
                #:fuzzy-match-p
                #:string-completion-rank)
  (:import-from :lem-lsp-mode
                #:convert-completion-response
                #:convert-completion-items)
  (:import-from :lem-lsp-base/type
                #:make-lsp-array)
  ;; les classes/readers viennent du package :lsp (nicknames de
  ;; :lem-lsp-base/protocol-3-17, protocol-3-17.lisp:4)
  (:import-from :lsp
                #:completion-item
                #:completion-list
                #:completion-list-items
                #:completion-item-label
                #:completion-item-sort-text
                #:completion-item-filter-text
                #:completion-item-detail)
  (:import-from :lem-lsp-base/converter
                #:convert-from-json
                #:convert-to-json)
  (:import-from :lem-lsp-base/yason-utils
                #:parse-json)
  (:import-from :yason
                #:encode)
  (:import-from :lem-tests/lsp-mode/test-utils
                #:make-test-server-capabilities
                #:make-test-spec
                #:make-test-workspace)
  (:import-from :lem-tests/lsp-mode/mock-client
                #:make-mock-client
                #:set-mock-client-response)
  (:export #:generate-completion-items
           #:run-benchmark
           #:*bench-seed*))

(in-package :lem-tests/lsp-mode/bench)

;;; ============================================================
;;; Génération synthétique DÉTERMINISTE
;;; ============================================================

(defparameter *bench-seed* 42)

(defun bench-random-state ()
  #+sbcl (sb-ext:seed-random-state *bench-seed*)
  #-sbcl (make-random-state t))

(defvar *prefixes* '("defun-" "defvar-" "defmethod-" "defclass-" "defmacro-"))
(defvar *suffixes* '("name" "value" "count" "list" "string"))

(defun generate-completion-items (n)
  "N items lsp:completion-item réalistes : ~50% avec filterText,
~30% avec detail, sortText zéro-paddé (ordre serveur dénormalisé).
Retourne un vecteur (lsp-array)."
  (let ((rnd (bench-random-state)))
    (make-lsp-array
     (loop :for i :below n
           :collect (let* ((prefix (nth (random 5 rnd) *prefixes*))
                           (suffix (nth (random 5 rnd) *suffixes*))
                           (label (format nil "~A~5,'0D-~A" prefix i suffix))
                           (filter-p (zerop (random 2 rnd)))
                           (detail-p (member (random 10 rnd) '(0 1 2)))
                           (sort-text (format nil "~6,'0D-~A" i label)))
                      ;; NOTE : construire AVEC tous les slots d'un coup, les
                      ;; writers restent bound. On ne met pas :documentation
                      ;; (coût markdown-buffer mesuré à part, optionnel).
                      (make-instance 'lsp:completion-item
                                     :label label
                                     :kind 3       ; completion-item-kind-function
                                     :sort-text sort-text
                                     :filter-text (when filter-p
                                                    (subseq label 0 (max 3 (1- (length label)))))
                                     :detail (when detail-p
                                               (format nil "Function (arg1 arg2) n=~D" i))))))))

(defun generate-completion-list-response (n)
  "Variante enveloppée dans lsp:completion-list (les DEUX branches de
convert-completion-response, lsp-mode.lisp:950-956, doivent être mesurées)."
  (make-instance 'lsp:completion-list
                 :is-incomplete nil
                 :items (generate-completion-items n)))

;;; ============================================================
;;; Infra de mesure
;;; ============================================================

(defun bytes-consed ()
  #+sbcl (sb-kernel::get-bytes-consed)
  #-sbcl 0)

(defun median (list)
  (let ((sorted (sort (copy-list list) #'<)))
    (if (oddp (length sorted))
        (nth (floor (length sorted) 2) sorted)
        (/ (+ (nth (1- (/ (length sorted) 2)) sorted)
              (nth (/ (length sorted) 2) sorted))
           2))))

(defstruct bench-result
  label min-ms median-ms alloc-mb)

(defun timed-run (fn &optional (repetitions 10))
  "2 warm-ups jetés, puis REPETITIONS mesures. GC full avant chaque run.
Min et médiane sur le run-time interne ; allocation du dernier run."
  (dotimes (_ 2) (funcall fn))
  (let ((runtimes '())
        (alloc-mb 0))
    (dotimes (_ repetitions)
      #+sbcl (sb-ext:gc :full t)
      
      ;; si trivial-garbage n'est pas dispo, gardez juste la branche sbcl :
      (let ((start (get-internal-run-time))
            (bytes-start (bytes-consed)))
        (funcall fn)
        (push (- (get-internal-run-time) start) runtimes)
        (setf alloc-mb (/ (max 0 (- (bytes-consed) bytes-start)) 1048576.0))))
    (let ((scale (/ 1000.0 internal-time-units-per-second)))
      (make-bench-result
       :label nil
       :min-ms (* (apply #'min runtimes) scale)
       :median-ms (* (median runtimes) scale)
       :alloc-mb alloc-mb))))

;;; ============================================================
;;; Point/buffer de test
;;; ============================================================

(defun call-with-bench-point (fn)
  "Buffer temporaire 'def ' au début, point à la fin du 'def', buffer supprimé
après. Le point est utilisé par convert-completion-items uniquement si les
items ont un textEdit (pas le cas ici)."
  (let ((buffer (make-buffer "*lsp-bench*")))
    (unwind-protect
         (progn
           (insert-string (buffer-point buffer) "def xyz")
           ;; recule derrière " xyz" pour simuler une complétion après "def"
           (buffer-end (buffer-point buffer))
           (funcall fn (buffer-point buffer)))
      (delete-buffer buffer))))

;;; ============================================================
;;; Scénarios
;;; ============================================================

;; ---- B1 : conversion brute (coût de convert-completion-items seul) ----
(defun bench-b1 (point items)
  (convert-completion-items point items))

;; ---- B2 : pipeline AVANT (actuel), fidèle à lsp-mode.lisp:972-977 ----
(defun bench-b2 (point response prefix)
  (if prefix
      (completion-strings prefix
                          (convert-completion-response point response)
                          :key #'lem/completion-mode::completion-item-label)
      (convert-completion-response point response)))

;; ---- B3 : pipeline APRÈS (contrat de l'optimisation) ----
(defun raw-item-filter-text (item)
  "handler-case comme lsp-mode.lisp:907 — le slot peut être bound à nil."
  (handler-case (lsp:completion-item-filter-text item)
    (unbound-slot () nil)))

(defun raw-item-sort-text (item)
  (handler-case (lsp:completion-item-sort-text item)
    (unbound-slot () nil)))

(defun b3-filter-raw (prefix items)
  "Filtre fuzzy sur les items BRUTS (zéro allocation d'objet métier).
filterText si présent, sinon label."
  (if (alexandria:emptyp prefix)
      (coerce items 'list)
      (loop :for item :across items
            :for text := (or (raw-item-filter-text item)
                             (lsp:completion-item-label item))
            :when (fuzzy-match-p prefix text)
              :collect item)))

(defun b3-rank-sort (prefix filtered)
  "Schwartzian transform : rank calculé UNE fois par item, puis deux
stable-sort (2e clé d'abord, 1re clé ensuite) = ordre composite lexicographique
(rank asc, sortText asc). Conserve l'ordre serveur à rank égal."
  (let ((decorated (mapcar (lambda (item)
                             (list (if (alexandria:emptyp prefix)
                                       0
                                       (string-completion-rank
                                        prefix (lsp:completion-item-label item)))
                                   (or (raw-item-sort-text item)
                                       (lsp:completion-item-label item))
                                   item))
                           filtered)))
    (setf decorated (stable-sort decorated #'string< :key #'second))
    (setf decorated (stable-sort decorated #'< :key #'first))
    (mapcar #'third decorated)))

(defun b3-after-pipeline (point response prefix n-limit)
  (let* ((raw (if (typep response 'lsp:completion-list)
                  (lsp:completion-list-items response)
                  response))
         (filtered (b3-filter-raw prefix raw))
         (ranked (b3-rank-sort prefix filtered))
         (top (subseq ranked 0 (min n-limit (length ranked)))))
    (convert-completion-items point top)))

;; ---- B4 : parse JSON → CLOS (le coût invisible côté transport) ----
(defun items-to-json-string (items)
  "Chemin réel du transport : convert-to-json (converter.lisp:139) puis
yason:encode, exactement ce que fait lem-language-client/request.lisp:85."
  (with-output-to-string (stream)
    (yason:encode (map 'list #'convert-to-json (coerce items 'list)) stream)))
    ;; NOTE : map 'list car yason encode bien les listes/hash-tables ;
    ;; si le vecteur passe tel quel, gardez (convert-to-json items) direct.

(defun bench-b4 (json-string)
  (let ((parsed (parse-json json-string)))
    (convert-from-json parsed '(lem-lsp-base/type:lsp-array lsp:completion-item))))

;; ---- B5 : completion-strings seule (isole le core, tous modes confondus) ----
(defun bench-b5 (labels)
  (completion-strings "def" labels))

;;; ============================================================
;;; Assertions fonctionnelles (le bench vérifie la correction)
;;; ============================================================

(defun assert-bench-invariants ()
  "Le contrat B3 doit tenir ; échoue bruyamment sinon."
  (call-with-bench-point
   (lambda (point)
     (let ((response (generate-completion-list-response 4000)))
       ;; 1) top-100 = exactement 100 items
       (let ((result (b3-after-pipeline point response "def" 100)))
         (assert (= 100 (length result)))
         ;; 2) tous les labels matchent fuzzy "def"
         (dolist (it result)
           (assert (fuzzy-match-p "def" (lsp:completion-item-label it))))
         ;; 3) ordre : ranks non-décroissants
         (let ((ranks (mapcar (lambda (it)
                                (string-completion-rank "def"
                                                        (lsp:completion-item-label it)))
                              result)))
           (assert (equal ranks (sort (copy-list ranks) #'<))
                   )
           ;; 4) à ranks égaux, sortText non-décroissant
           (loop :for (a b) :on result
                 :do (when (= (string-completion-rank "def" (lsp:completion-item-label a))
                              (string-completion-rank "def" (lsp:completion-item-label b)))
                       (assert (string<= (or (raw-item-sort-text a)
                                             (lsp:completion-item-label a))
                                         (or (raw-item-sort-text b)
                                             (lsp:completion-item-label b))))))))))))

;;; ============================================================
;;; Runner + affichage
;;; ============================================================

(defun print-line (label min median alloc-mb)
  (format t "~40A ~10,2F ~10,2F ~10,1F~%" label min median alloc-mb))

(defun run-benchmark (&optional (sizes '(100 1000 4000)) (repetitions 10))
  "Affiche un tableau aligné (texte) ; vérifie les invariants d'abord."
  (assert-bench-invariants)
  (format t "~%LSP completion benchmark - ~A ~A~%"
          (lisp-implementation-type) (lisp-implementation-version))
  
  (format t "dataset: seed=~D, sortText zero-padded, 50% filterText, 30% detail~%"
          *bench-seed*)
  (format t "~40A ~10A ~10A ~10A~%" "scenario" "min-ms" "median-ms" "alloc-MB")
  (dolist (n sizes)
    (format t "---- n = ~D ----~%" n)
    (let* ((items (generate-completion-items n))
           (response (generate-completion-list-response n))
           ;; le JSON est sérialisé UNE fois, hors mesure du parse
           (json (items-to-json-string items)))
      
      ;; B1
      (call-with-bench-point
       (lambda (point)
         (let ((r (timed-run (lambda () (bench-b1 point items)) repetitions)))
           (print-line (format nil "B1 convert all (~D)" n)
                       (bench-result-min-ms r)
                       (bench-result-median-ms r)
                       (bench-result-alloc-mb r)))))
      ;; B2
      (call-with-bench-point
       (lambda (point)
         (let ((r (timed-run (lambda () (bench-b2 point response "def")) repetitions)))
           (print-line (format nil "B2 BEFORE convert+fuzzy+rank (~D)" n)
                       (bench-result-min-ms r)
                       (bench-result-median-ms r)
                       (bench-result-alloc-mb r)))))
      ;; B3
      (call-with-bench-point
       (lambda (point)
         (let ((r (timed-run (lambda () (b3-after-pipeline point response "def" 100))
                             repetitions)))
           (print-line (format nil "B3 AFTER filter+rank+take100+conv (~D)" n)
                       (bench-result-min-ms r)
                       (bench-result-median-ms r)
                       (bench-result-alloc-mb r)))))
      ;; B4 — parse : la sérialisation est faite DANS le run (coût convert-to-json
      ;; est déjà mesuré séparément), on mesure seulement parse+conversion.
      (let ((r (timed-run (lambda () (bench-b4 json)) repetitions)))
        (print-line (format nil "B4 parse JSON->CLOS (~D items)" n)
                    (bench-result-min-ms r)
                    (bench-result-median-ms r)
                    (bench-result-alloc-mb r)))
      ;; B5
      (let* ((labels (map 'list #'lsp:completion-item-label items))
             (r (timed-run (lambda () (bench-b5 labels)) repetitions)))
        (print-line (format nil "B5 completion-strings core (~D)" n)
                    (bench-result-min-ms r)
                    (bench-result-median-ms r)
                    (bench-result-alloc-mb r))))))
