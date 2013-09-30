;;; masq.el --- Live images.  -*- lexical-binding: t -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Huaiyuan <huaiyuan@gmail.com>
;; Version: 0.1
;; Package-Requires: ((teximg "0") (sexptex "0"))
;; Keywords: lisp sexp preview image

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides facilities for masking portions of the buffer
;; (identified by regexps) with images computed from the underlying
;; contents. For example, one may get on-the-fly preview of TeX snippets
;; using the following rules:
;; 
;;     (font-lock-add-keywords nil
;;       '(("\\$[^$]+?\\$" (0 (masq-face 'tex-to-img) append))))
;; 
;; provided `tex-to-img' is a function that will generate and return
;; images from TeX snippets identified by the regexp. 
;;
;; This file also includes `masq-mode', a minor mode with a few prebuilt
;; rules.
;; 
;; Compared with auctex's `preview', `masq-mode' does not require
;; dedicated key-binding to toggle activation of previewing; instead,
;; when the cursor moves into a region marked by a rule, the masking
;; image will be removed and the underlying buffer text revealed so that
;; the user can edit it. When the cursor leaves the region, a (perhaps
;; newly computed if text modified) image will be automatically
;; installed again.

;; * Implementation details:
;; In increasing frequencies being called:
;; 1. masq-point (post-command)
;; 2. masq-update (font-lock rule fires)
;; 3. masq-dispatch (redisplay)


;;; Code:

(require 'teximg)
(require 'sexptex)
(require 'url)

(defvar masq-overlay nil)

(defvar masq-point 1)

(defun masq-point ()
  "Cleanup live editing display; meant for `post-command-hook'.
Note: need to keep track of user initiated movement of POINT here
since `font-lock-mode' will move point around."
  (setq masq-point (point))
  (when (or t (< masq-point (overlay-start masq-overlay))
            (< (overlay-end masq-overlay) masq-point))
    (overlay-put masq-overlay 'after-string "")))

(defun masq-update-overlay (o i a b)
  "Assume: point in range, live-editing mode."
  (let* ((i (or i (get-text-property a 'data)))
         (p (save-excursion (goto-char b) (point-at-eol)))
         (img (cond
                ((stringp i) i)
                ((consp i) (propertize " " 'display (append i '(:margin 2))))
                (t "...")))
         (foo (propertize (concat img "\n") 'face 'default)))
    (overlay-put o 'after-string foo)
    (move-overlay o p (1+ p))))

(defun masq-wrapup (overlay data)
  "Install display data and cleanup; for callback."
  (let* ((start (overlay-start overlay))
         (end (overlay-end overlay))
         (file1 (getf (cdr-safe (get-text-property start 'data)) :file))
         (file2 (getf (cdr-safe data) :file)))
    (when (and file1 (not (string= file1 file2))) (delete-file file1))
    (with-silent-modifications
      (put-text-property start end 'data data)
      (delete-overlay overlay))))

(defun masq-update (start end fn)
  "Update the image and store it in cache if content modified and
previous update is done."
  (unless (find-if (lambda (o) (overlay-get o 'lock)) (overlays-in start end))
    (let ((sha (sha1 (current-buffer) start end)))
      (unless (string= sha (get-text-property start 'sha))
        (put-text-property start end 'sha sha)
        (let ((o (make-overlay start end)))
          (overlay-put o 'lock t)
          (condition-case nil
              (funcall fn start end (lambda (x) (masq-wrapup o x)))
            (error (delete-overlay o))))))))

(defun masq-dispatch (a b p)
  "Test if point is in range; will be called by redisplay
frequently, so the first branch needs to be fast."
  (or (< p a) (<= b p)
      (prog1 nil (masq-update-overlay
                  masq-overlay (get-text-property a 'data) a b))))

;;;###autoload
(defun masq-face (fn)
  "Return face to be use by `font-lock-mode'; will be called whenever
`font-lock-mode' updates.  Contexts: (1)."
  (let* ((a (match-beginning 0)) (b (match-end 0)))
    (masq-update a b fn)
    `(face highlight
      display
      (when (masq-dispatch ,a ,b masq-point)
        ,@(get-text-property a 'data)))))


;;; Application
(defun masq-eval-0 (a b cont)
  (let ((x (eval (read (buffer-substring-no-properties (1+ a) (1- b))))))
    (funcall cont (if (stringp x) x (prin1-to-string x)))))

(defun masq-eval (a b cont)
  (funcall cont (eval (save-excursion (goto-char (min a b)) (sexp-at-point)))))

(defun masq-tex (a b cont)
  (let ((fg (save-excursion (goto-char a) (foreground-color-at-point))))
    (funcall cont (teximg (buffer-substring-no-properties a b) nil fg))))

(defun masq-sexp (a b cont)
  (funcall cont (teximg (sexptex-text (buffer-substring-no-properties a b)))))

(defun masq-math (a b cont)
  (ignore b)
  (save-excursion
   (goto-char a)
   (let ((fg (foreground-color-at-point))
         (end (progn (forward-sexp) (1- (point)))))
     (funcall cont (teximg (buffer-substring-no-properties (1+ a) end)
                           nil fg)))))

(defun masq-matcher (regexp)
  "Return a matcher for `font-lock-keywords' matching sexps that
immediately precede the given REGEXP."
  (let ((regexp (concat "\\s) *" regexp)))
    (lambda (limit)
      (when (re-search-forward regexp limit t)
        (ignore-errors
          (save-excursion
            (let ((p (1+ (match-beginning 0))))
              (goto-char p)
              (backward-sexp 1)
              (put-text-property (point) p 'font-lock-multiline t)
              ;; (set-match-data (cons (point) (cdr (match-data t))))
              (set-match-data (list (point) (match-end 0) (current-buffer)))))
          (point))))))

(defvar masq-keywords
  `(("=+(.*?)=" (0 (masq-face 'masq-eval-0) append))
    ("\\$[^$]+?\\$" (0 (masq-face 'masq-tex) append))
    (,(masq-matcher ";e") (0 (masq-face 'masq-eval) append))
    (,(masq-matcher ";m") (0 (masq-face 'masq-sexp) append))
    (,(masq-matcher ";dm") (0 (masq-face 'masq-math) append))))

;;;###autoload
(define-minor-mode masq-mode
  "Present some lisp symbols using Unicode characters."
  :lighter " MsQ"
  (cond
   (masq-mode
    (make-local-variable 'font-lock-extra-managed-props)
    (mapc (lambda (x) (add-to-list 'font-lock-extra-managed-props x))
          '(display point-entered point-left lock sha))
    (set (make-local-variable 'masq-overlay)
         (make-overlay (point-min) (1+ (point-min))))
    (make-local-variable 'masq-point)
    (add-hook 'post-command-hook 'masq-point nil t)
    (font-lock-add-keywords nil masq-keywords 'append)
    (font-lock-fontify-buffer))
   (t
    (delete-overlay masq-overlay)
    (remove-hook 'post-command-hook 'masq-point t)
    (font-lock-remove-keywords nil masq-keywords)
    (font-lock-unfontify-buffer) (font-lock-fontify-buffer)
    (kill-local-variable 'font-lock-extra-managed-props))))


;;; Test
(eval-when (eval)
  (defvar *keywords* nil)
  (fset 'matcher (masq-matcher ";"))
  (let ((keywords `(("=+(.*?)=" (0 (masq-face 'masq-eval-0) append))
                    ("\\$+.*?\\$+" (0 (masq-face 'masq-tex) append))
                    (,(masq-matcher ";") (0 (masq-face 'masq-eval))))))
    (font-lock-remove-keywords nil *keywords*)
    (setf *keywords* keywords)
    (font-lock-add-keywords nil *keywords* 'append)
    (font-lock-fontify-buffer)))

(autoload 'mm-dissect-buffer "mm-decode" "Dissect to return MIME handles.")
(eval-when (compile load eval) (require 'mm-decode))

(defun masq-retrieve (url callback &optional cbargs)
  "Alternative to `url-retrieve' using `wget'."
  (let* ((process-connection-type nil)
         (proc (start-process "wget" nil "wget" "-O" (car cbargs) url)))
    (set-process-sentinel proc (lambda (p s) (ignore p s)
                                 (apply callback nil cbargs)))))

(defun masq-cache (url &optional callback)
  (let* ((urlo (url-generic-parse-url url))
         (file (if (string= "file" (url-type urlo))
                   (url-filename urlo)
                 (expand-file-name (sha1 url) temporary-file-directory)))
         (fn (lambda (stat file)
               (unless (eq :error (caar stat))
                 (let ((handle (mm-dissect-buffer t)))
                   (mm-save-part-to-file handle file)
                   (kill-buffer)
                   (mm-destroy-parts handle)
                   (when callback
                     (funcall callback file))))))
         (gn (lambda (stat file) (ignore stat file)
               (when callback (funcall callback file)))))
    (ignore fn)
    (cond ((file-exists-p file) file)
          (t (masq-retrieve url gn (list file)) nil))))

(defun masq-image (url)
  (let* ((a (match-beginning 0))
         (b (match-end 0))
         (fn (lambda (file)
               (let ((i (create-image file)))
                 (with-silent-modifications
                   (put-text-property a b 'data i)
                   ;; (put-text-property a b 'display i)
                   ))))
         (file (masq-cache url fn)))
    (if (and file (ignore-errors (image-type file)))
        (create-image file)
      "Retrieving...")))

(eval-when ()
  (+ (- (* (/ (sqrt 2x) -17)
           (exp (+ 3 b))))
     (sqrt (+ c (expt (f b) 3) a)))     ;m
  (let ((b (- x y z)))
    (* pi (expt b 3)))                  ;m
  "$\int^b_a \sin x dx$"
  "$\alpha \Rightarrow \beta$"
  "$\frac{x}{z}$"
  "$f(g(x^w_2+\pi))$"
  "$\sum_{i \in \mathbf{Z}} \frac{e^{\pi i}}{\Gamma^3}$"
  (format-time-string "%Y-%T")                                    ;e
  (symbol-value 'default-directory)                               ;e
  (propertize "Fishes" 'face 'bold)                               ;e
  (create-image "/home/yuan/doc/www/images/hunchentoot.gif")      ;e
  (masq-image "file:///home/yuan/doc/www/images/hunchentoot.gif") ;e
  (masq-image "http://thumbs.reddit.com/t3_jljgd.png") ;e
  (let* ((cat (floor (+ (expt x (expt y z)) (expt (expt x y) z))))
         (b (* (- x (- y z)) (- (- x y) z)))
         (a (+ (max (/ b (expt x (+ y 22))) (+ 5 (+ b (log x 2)))
                    (exp (+ z cat))))))
    (cond ((evenp cat) (sqrt (+ (expt a 2) (expt b 3))))
          ((plusp a) (* (- (+ 2 a)) (- (* 2 a))) 
           (expt a (expt b cat)))
          ((loop for i below a sum i)))) ;m
  )
;; Testing: $\sqrt \frac{\alpha}{2}$ and $\frac{\partial\mu}{\partial x}\leftarrow\Gamma(\nu)$.
;; Test if $\alpha \rightarrow \beta \over \sqrt \delta$ is handled properly $\Gamma \Leftarrow \Delta$.
