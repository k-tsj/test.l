;;;
;;; test.l
;;;
;;;   Copyright (c) 2004-2010 Kazuki Tsujimoto, All rights reserved.
;;;
;;;   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
;;;   (gauche.test in Gauche 0.8.6)
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;;;
;;; Usage:
;;;
;;; Put this file on $XYZZY/site-lisp, and add following lines to
;;; your .xyzzy.
;;;
;;;   (require "test")
;;;   (use-package "test")
;;;
;;; Writing your own test (site-lisp/test-mylib.l) for your library (site-lisp/mylib.l).
;;;
;;;   (load-library "mylib") ;; force reload
;;;   (in-package "mylib") ;; if you need
;;;   (use-package "test")
;;;   
;;;   (test-start "mylib")
;;;   (test-section "Section 1")
;;;   (test  "test-1.0" '(0) #'(lambda () '(0)))
;;;   (test  "test-1.1" '(0) #'(lambda () '(0)) #'eq) ;; fail
;;;   (test* "test-1.2" '(0) '(0))
;;;   
;;;   (test-section "Section 2")
;;;   (test* "test-2.0" *test-error* (/ 1 0)) ;; pass
;;;   (test* "test-2.1" 0 (/ 1 0)) ;; fail
;;;   
;;;   (setq *test-report-error* t) ;; with stack trace
;;;   (test* "test-2.2" 0 (/ 1 0)) ;; fail
;;;   
;;;   (test-end)
;;; 
;;; To run test, open test file (test-mylib.l and mylib.l are both ok) and use M-x run-test.
;;; You could also bind the command to a key as follows.
;;;
;;;   (define-key ed::*lisp-mode-map* '(#\C-c #\C-t) 'run-test)
;;;

(provide "test")
(defpackage "test"
  (:use "lisp" "editor"))
(in-package "test")

(export '(test test* test-start test-end test-section test
          *test-error* *test-report-error* test-error-p prim-test
          *test-output-buffer-name* run-test *test-output-buffer-keyword-list*))

;; Options ----------------------------------------------------------
(defvar *test-output-buffer-name* "*test*")
(defvar *test-output-buffer-keyword-list*
  (compile-regexp-keyword-list
   '(("^passed\\." nil (:color 1))
     ("^failed\\." nil (:color 2)))))

;; To enable this option, you must set it true after test-start.
(defvar *test-report-error* nil) 

;; Condition Definition ---------------------------------------------
(define-condition test-error (error) (mes)
  (:report (lambda (c s)
             (format s "Test Error: ~A" (test-error-mes c)))))

(defconstant *test-error* (make-condition 'test-error :mes nil))

;; kludge
(defun test-error-p (e)
  (and (eq 'structure (type-of e))
       (string-match "^Test Error: " (format nil "~A" e))))

;; Tests ------------------------------------------------------------
(defun wrap-comp (comp)
  #'(lambda (x y)
      (if (and (test-error-p x)
               (test-error-p y))
          t
        (funcall comp x y))))

;; Primitive test. 
(defun prim-test (msg expect thunk &optional compare)
  (let ((cmp (wrap-comp (or compare #'equal))))
    (format *output-stream* "test ~A, expects ~S ==> " msg expect)
    (let ((r (funcall thunk)))
      (if (funcall cmp expect r)
          (format *output-stream* "ok\n")
          (progn
            (format *output-stream* "ERROR: GOT ~S\n" r)
            (setq *discrepancy-list*
                  (cons (list msg expect r) *discrepancy-list*))))
      )))

;; Normal test.
(defun test (msg expect thunk &optional compare)
  (when *test-report-error*
    (start-trace-capture))
  (unwind-protect
      (funcall #'prim-test msg expect
               #'(lambda ()
                   (handler-case
                       (funcall thunk)
                     (error (e)
                       (make-condition 'test-error :mes e))))
               compare)
    (when *test-report-error*
      (end-trace-capture))))

;; A convenient macro version
(defmacro test* (msg expect form &rest compare)
  `(test ,msg ,expect #'(lambda () ,form) ,@compare))

;; Logging and bookkeeping ------------------------------------------
(defvar *preserved-trace-status* nil)

(defun start-trace-capture ()
  (setq *preserved-trace-status* (list *error-output* si:*trace-on-error*))
  (setq *error-output* *output-stream*)
  (setq si:*trace-on-error* t))

(defun end-trace-capture ()
  (setq *error-output* (first *preserved-trace-status*))
  (setq si:*trace-on-error* (second *preserved-trace-status*)))

(defvar *output-stream* nil)
(defvar *discrepancy-list* nil)

(defun initialize-output-stream ()
  (close-output-stream)
  (unless (find-buffer *test-output-buffer-name*)
    (when (= (count-windows) 1)
      (split-window))
    (other-window))
  (let ((sbuf (selected-buffer))
        (tbuf (get-buffer-create *test-output-buffer-name*)))
    (set-buffer tbuf)
    (erase-buffer tbuf)
    (make-local-variable 'need-not-save)
    (setq need-not-save t)
    (set-buffer sbuf)
    (setq *output-stream* (make-buffer-stream tbuf nil nil))))

(defun close-output-stream ()
  (when *output-stream*
    (close *output-stream*)
    (setq *output-stream* nil)))

(defun test-start (msg)
  (initialize-output-stream)
  (setq *test-report-error* nil)
  (format *output-stream* "Testing ~A ... \n" msg)
  (setq *discrepancy-list* '()))

(defun make-string (k &optional char)
  (let ((c (or char #\SPC)))
    (make-sequence 'string k :initial-element c)))

(defun test-section (msg)
  (let ((msglen (length msg)))
    (format *output-stream* "<~A>~A\n" msg (make-string (max 5 (- 77 msglen)) #\-))))

(defun buffer-visible-in-window-p (buffer)
  (member buffer (mapcar #'cadr (caddr (current-window-configuration))) :test #'equal))

(defun test-end ()
  (defun fmt (&rest args)
    (apply #'format *output-stream* args))
  (if (null *discrepancy-list*)
      (fmt "passed.\n")
    (progn
      (fmt "failed.\ndiscrepancies found.  Errors are:\n")
      (dolist (r (reverse *discrepancy-list*) t)
        (apply #'fmt "test ~A: expects ~S => got ~S\n" r))))
  (fmt "\n~A" (format-date-string "%Y/%m/%d %H:%M:%S%Z" (get-universal-time)))
  (close-output-stream)
  (let ((buf (find-buffer *test-output-buffer-name*)))
    (unless (buffer-visible-in-window-p buf)
      (switch-to-buffer buf)
      (make-local-variable 'regexp-keyword-list)
      (setq regexp-keyword-list *test-output-buffer-keyword-list*))
    (with-selected-window
      (let ((win (get-buffer-window (find-buffer *test-output-buffer-name*))))
        (when win
          (set-window win)
          (goto-char (point-max)))))))

;; Runner -----------------------------------------------------------
(defun run-test ()
  (interactive)
  (let ((module-name (get-module-name (get-buffer-file-name))))
    (when module-name
      (load-library (if (test-module-p module-name)
                        module-name
                      (get-test-module-name module-name))))))

(defun get-test-module-name (target-module-name)
  (let ((file-ns (file-namestring target-module-name))
        (dir-ns (substring target-module-name 0 (string-match "[^/]+$" target-module-name))))
    (concat dir-ns "test-" file-ns)))

(defun get-module-name (file-path)
  (defun normalize-path (path)
    (map-backslash-to-slash (truename path)))
  (defun in-basedir-p (basedir file-path)
    (string-match (regexp-quote basedir) file-path))
  (defun remove-extention (path)
    (substring path 0 (string-match "\\.[^.]+$" path)))
  
  (let ((basedir (normalize-path (merge-pathnames "site-lisp" (si:system-root))))
        (file-path (normalize-path file-path)))
    (if (in-basedir-p basedir file-path)
        (remove-extention (string-left-trim "/" (substitute-string file-path basedir "")))
      nil)))

(defun test-module-p (module-name)
  (string-match "^test-" (file-namestring module-name)))
