;;; sexptex.el --- Convert S-expression to TeX.  -*- lexical-binding: t -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Huaiyuan <huaiyuan@gmail.com>
;; Version: 0.1
;; Keywords: lisp sexp tex conversion

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

;; Convert S-expressions to TeX formulas.


;;; Code:

(require 'cl)

(defvar *sexptex-scratch*
  (let ((pathname (concat temporary-file-directory ".sexptex/")))
    (make-directory pathname t)
    pathname))

(defvar *sexptex-infix*
  '((expt 4 right)
    (^    4 right)
    (*    3 left )
    (/    3 left )
    (+    2 left )
    (-    2 left )
    (=    1 left )
    (<    1 left )
    (>    1 left )
    (<=   1 left )
    (>=   1 left )
    (and  0 left )
    (or   0 left )))

(defvar *sexptex-table*
  '((* "\\cdot") (/= "\\neq") (not "\\neg") (expt "^") (aref "_")
    (and "\\wedge") (or "\\vee" (<= "\\leq") (>= "\\geq"))
    log sin cos tan min max sqrt lim sum 
    alpha beta gamma delta epsilon zeta eta theta
    iota kappa lambda mu nu xi omicron pi rho
    varsigma sigma tau upsilon varphi chi psi omega
    Alpha Beta Gamma Delta Epsilon Zeta Eta Theta
    Iota Kappa Lambda Mu Nu Xi Omicron Pi Rho
    Varsigma Sigma Tau Upsilon Varphi Chi Psi Omega))

(defvar *sexptex-hints*
  '(( defun     . (name args body))
    ( lambda    . (args body))
    ( let       . (bindings body))
    ( let*      . (bindings body))
    ( cond      . (clauses))
    ( case      . (name clauses))
    ( ecase     . (name clauses))
    ( typecase  . (name clauses))
    ( etypecase . (name clauses))
    ( do        . (bindings clause body))
    ( do*       . (bindings clause body))
    ( labels    . (clauses body))))

(defun sexptex-escape (string)
  ;;(replace-regexp-in-string "\\([%&]\\)" "\\\\1" string)
  (with-output-to-string
    (loop for char across string do
          (if (= ?\\ char) (princ "\\textbackslash ")
            (if (find char "%&{}^_#$")
                (princ (format "\\char`\\%c{}" char))
              (write-char char))) )))

(defun sexptex-concat (sequence separator &optional
                                precedence newline)
  (with-output-to-string
    (loop for x in sequence for y on sequence do
          (cond ((and (consp x) (eq :sp (car x)))
                 (when newline (princ (sexptex-sexp x nil t))))
                ((eq y sequence)
                 (princ (sexptex-sexp x precedence newline 'left)))
                ((let ((role (if (cdr y) t 'right)))
                   (princ separator)
                   (princ (sexptex-sexp x precedence newline role))))))))

(defun sexptex-literal-1 (form)
  (labels
      ((p (x &optional c)
          (cond ((null x) (format "(%s)" (or c "")))
                ((atom x) (let ((x (sexptex-escape (format "%S" x))))
                            (if c (format "(%s . %s)" c x) x)))
                ((and (not c) (eq :sp (car x)))
                 (format "}\\right.\\\\\n&\\left.\\texttt{\\hspace{%sex}"
                         (cadr x)))
                ((p (cdr x) (concat (or c "") (if c " " "") (p (car x))))))))
    (format "\\left.\\texttt{%s}\\right." (p form))))

(defun sexptex-literal (form)
  (labels
      ((p (x &optional cc)
          (cond ((null x) (if cc (q cc) (sexptex-atom x)))
                ((atom x) (let ((x (sexptex-atom x)))
                            (if cc (q (cons (concat "\\cdot " x) cc)) x)))
                ((and (not cc) (eq :sp (car x))) x)
                ((p (cdr x) (cons (p (car x)) cc)))))
       (q (s)
          (let ((s (member-if #'stringp (nreverse s))))
            (if (every #'stringp s)
                (format "\\left[%s\\right]" (mapconcat #'identity s "\\;"))
              (format
               "\\left[\n\\begin{aligned}\n&%s\n\\end{aligned}\\right]"
               (with-output-to-string
                 (loop for i in s for j = nil then k for k = (stringp i) do
                       (princ (if k (if j (concat "\\;" i) i)
                                (format "\\\\\n&\\hspace{%dex}"
                                        (1- (cadr i))))))))))))
    (p form nil)))

(defun sexptex-bindings (clauses)
  (let ((fp (lambda (b)
              (if (consp b)
                  (destructuring-bind (var . val) b
                    (if (eq var :sp) ""
                      (format "%s &: %s \\\\\n"
                              (sexptex-sexp var)
                              (sexptex-concat val "\\textbf{ then }" nil t))))
                (format "%s &: nil \\\\\n" b)))))
    (format "\\\\\n&\\quad\\left\\{\\begin{aligned}\n%s\\end{aligned}\\right."
            (mapconcat fp clauses " "))))

(defun sexptex-clause (clause)
  (if (consp clause)
      (let ((var (car clause))
            (val (cdr clause)))
        (cond ((eq var :sp) "\\\\\n")
              (val
               (format "&\\left\\{\\begin{aligned}\n\
                               &%s \\Rightarrow %s\n\
                               \\end{aligned}\\right."
                       (sexptex-sexp var)
                       (sexptex-concat val "{}" nil t)))
              ((format "&\\left\\{\\begin{aligned}\n\
                               &%s\n\\end{aligned}\\right."
                       (sexptex-sexp var)))))
    (format "&%s" clause)))

(defun sexptex-clauses (clauses)
  (format "\\\\\n%s " (mapconcat #'sexptex-clause clauses " ")))

(defun sexptex-body (body)
  (format "{}&%s{}" (sexptex-concat body "\n" nil t)))

(defun sexptex-expand (lambda-list head form)
  (format
   "\\boxed{%%\n\\begin{aligned}\n&\\vspace{0pt}%s\n%s\\end{aligned}}"
   (format "\\textbf{%s}\\;" head)
   (with-output-to-string
     (loop with val = nil with tail = form
           for (var . more) on lambda-list
           when more do
           (loop for k on tail          ; for (i . j) on tail
                 for i = (car k)
                 for j = (cdr k)
                 when (and (consp i) (eq :sp (car i)))
                 do (princ "\\\\\n")
                 else return (setf val i tail j))
           else do (setf val tail)
           do (princ (case var
                       (name (format "\\textsf{%s}\\;" val))
                       (args (format "\\left[%s\\right]"
                                     (sexptex-concat val "\\;")))
                       (bindings (sexptex-bindings val))
                       (clause (sexptex-clause val))
                       (clauses (sexptex-clauses val))
                       (body (sexptex-body val))))))))

(defun sexptex-atom (form)
  (typecase form
    (string (format "\\texttt{\"%s\"}" (sexptex-escape form)))
    (symbol
     (let* ((info (find form *sexptex-table*
                        :key (lambda (x) (if (consp x) (car x) x)))))
       (typecase info
         (null (let ((tex (sexptex-escape (format "%s" form))))
                 (if (= 1 (length tex))
                     (format "{%s}" tex)
                   (format "\\textit{%s}" tex))))
         (atom (concat "\\" (symbol-name info)))
         (cons (second info))
         (t    (format "{%s}" (sexptex-escape form))))))
    (t (format "{%s}" form))
    (string (format "\\verb|\"%s\"|" form))))

(defun sexptex-compound-regular (head tail &optional
                                      precedence newline role)
  (let ((info (find head *sexptex-infix* :key #'car))
        (prec (or precedence 0))
        (role (or role 'left)))
    (cond
     ((and info)                        ; infix
      (destructuring-bind (h p a) info (ignore h)
        (cond ((cdr tail)
               (let* ((tex (sexptex-concat tail (sexptex-sexp head) p))
                      (bra (or (< p prec) (and (= p prec) (not (eq a role))))))
                 (format (if bra "{\\left(%s\\right)}" "{%s}") tex)))
              (t (format "{%s %s}" (sexptex-sexp head)
                         (sexptex-sexp (car tail) (1+ p)))))))
     ((and (symbolp head) (fboundp head) (not (functionp head)))
      (let ((hints (cdr (assoc head *sexptex-hints*))))
        (cond (hints (sexptex-expand hints head tail))
              ((format
                "\\boxed{\\begin{aligned}&\\textbf{%s}\\;%s\\end{aligned}}"
                head (sexptex-concat tail "\\;" precedence t))))))
     ((and nil newline)                 ; function application
      (format "\\left[\\begin{aligned}&%s\\;%s\\end{aligned}\\right]"
              (sexptex-sexp head precedence newline)
              (sexptex-concat tail "\\;" precedence newline)))
     (newline                           ; function application
      (sexptex-literal (cons head tail)))
     (t                                 ; function application
      (format "%s(%s)"
              (sexptex-sexp head) (sexptex-concat tail ","))))))

(defun sexptex-compound (head tail &optional precedence newline role)
  (case head
    (("" :sp) (if newline (format "\\\\\n&\\hspace{%sex}" 2) "\\;"))
    (tex (car tail))
    (quote (format "{}'%s" (sexptex-literal (car tail))))
    (\` (format "{}`%s" (sexptex-literal (car tail))))
    (aref (format "{{%s}_{%s}}"
                  (sexptex-sexp (car tail)) (sexptex-concat (cdr tail) ",")))
    (sqrt (format "\\sqrt{%s}" (sexptex-sexp (car tail))))
    (floor (if (cdr tail)
               (format "\\lfloor\\frac{%s}{%s}\\rfloor"
                       (sexptex-sexp (car tail))
                       (sexptex-sexp (cadr tail)))
             (format "\\lfloor{%s}\\rfloor" (sexptex-sexp (car tail)))))
    (exp (format "{}e^{%s}" (sexptex-sexp (car tail))))
    (log (format "\\log_{%s}\\left(%s\\right)"
                 (if (cdr tail) (sexptex-sexp (second tail)) "")
                 (sexptex-sexp (car tail))))
    (/ (if (cdr tail) (format "\\frac{%s}{%s}"
                              (sexptex-sexp (car tail))
                              (sexptex-concat (cdr tail) "\\cdot"))
         (format "\\frac{1}{%s}" (sexptex-sexp (car tail)))))
    (= (format "\\begin{aligned}\n%s &= %s\n\\end{aligned}"
               (sexptex-sexp (car tail))
               (sexptex-concat (cdr tail) "\\\\\n& = ")))
    (t (sexptex-compound-regular head tail precedence newline role))))

(defun sexptex-sexp (form &optional precedence newline role)
  (if (consp form)
      (sexptex-compound (car form) (cdr form) precedence newline role)
    (sexptex-atom form)))

;;;###autoload
(defun sexptex-text (string)
  "Return TeX snippet given STRING containing textual
representation of a sexp."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\n\\( +\\)" nil t)
      (let* ((q (current-column))
             (p (save-excursion (backward-up-list 1) (current-column)))
             (x (format "(:sp %d)\n%s" (- q p) (match-string 1))))
        (insert x)))
    (goto-char (point-min))
    (let ((sexp (read (current-buffer))))
      (concat "\\begin{align*}\n"
              (sexptex-sexp sexp)
              "\n\\end{align*}"))))

(provide 'sexptex)
