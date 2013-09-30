;;; teximg.el --- Convert TeX snippet to image.  -*- lexical-binding: t -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Huaiyuan <huaiyuan@gmail.com>
;; Version: 0.1
;; Keywords: lisp sexp tex image conversion

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

;; Convert strings containing TeX snippets to rendered images.


;;; Code:

(require 'cl)
(require 'color)

(defvar *teximg-scratch*
  (let ((pathname (expand-file-name "teximg" temporary-file-directory)))
    (make-directory pathname t)
    (file-name-as-directory pathname)))

(defun teximg-dvi-color (color)
  (apply 'format "rgb %g %g %g"
         (mapcar (lambda (x) (/ x 65535.0)) (color-values color))))

(defvar teximg-foreground
  (frame-parameter nil 'foreground-color))

(defvar teximg-background
  (frame-parameter nil 'background-color))

(defvar teximg-dvi-shadow
  (teximg-dvi-color (face-attribute 'shadow :foreground)))

(defvar teximg-preamble
  "\\documentclass{article}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage[mathscr]{eucal}
\\pagestyle{empty}")

(defvar teximg-preamble-extra
  (format "\n
\\makeatletter
\\renewcommand{\\boxed}[1]{\\special{color push %s}{%%
\\fbox{\\special{color pop}\\m@th$\\displaystyle#1$
\\special{color push %s}}}\\special{color pop}}
\\makeatother" teximg-dvi-shadow teximg-dvi-shadow))

(defvar teximg-log nil
  ;;(list :file (concat *teximg-scratch* "teximg.log"))
  "The destination for output of `call-process'.")

(defun teximg-string-to-tex (string ofile)
  "Return filename of a tex file created from tex snippet in string."
  (with-temp-file ofile
    (insert teximg-preamble)
    (insert teximg-preamble-extra)
    (insert "\n\\begin{document}\n\\Large\n" string "\n\\end{document}\n"))
  ofile)

(defun teximg-tex-to-dvi (ifile)
  (let ((ofile (concat (file-name-sans-extension ifile) ".dvi")))
    (call-process "latex" nil teximg-log nil ifile)
    (cond ((file-newer-than-file-p ifile ofile)
           (error "Failed to create dvi file from %s" ifile))
          (ofile))))

(defun teximg-dvi-to-png (ifile dpi fg bg)
  (let ((ofile (concat (file-name-sans-extension ifile) ".png")))
    (call-process "dvipng" nil teximg-log nil
                  "-fg" fg "-bg" bg
                  "-D" (number-to-string dpi)
                  "-T" "tight" "--gamma" "4" "-o" ofile
                  ifile)
    (cond ((file-newer-than-file-p ifile ofile)
           (error "Failed to create png file from %s" ifile))
          (ofile))))

(defun teximg-tex-to-pdf (ifile)
  (let ((ofile (concat (file-name-sans-extension ifile) ".pdf")))
    (call-process "pdflatex" nil teximg-log nil ifile)
    (cond ((file-newer-than-file-p ifile ofile)
           (error "Failed to create dvi file from %s" ifile))
          (ofile))))

(defun teximg-pdf-to-png (ifile dpi)
  (let ((ofile (concat (file-name-sans-extension ifile) ".png")))
    (call-process "pdfdraw" nil teximg-log nil
                  "-r" (number-to-string dpi)
                  "-o" ofile
                  ifile)
    (cond ((file-newer-than-file-p ifile ofile)
           (error "Failed to create png file from %s" ifile))
          (ofile))))

(defun teximg-compile (string &optional scale foreground background)
  "Return filename of image produced by compiling STRING as LaTeX snippet."
  (let* ((default-directory *teximg-scratch*)
         (tfile (expand-file-name "temp.tex"))
         (ofile (expand-file-name (concat (sha1 string) ".png"))))
    (cond ((or scale (not (file-exists-p ofile)))
           (clear-image-cache ofile)
           (let* ((scale (* (or scale 1.0) ;(face-attribute 'default :height)
                            7.227 (font-get (font-at (point)) :size)))
                  (fg (teximg-dvi-color (or foreground teximg-foreground)))
                  (bg (teximg-dvi-color (or background teximg-background)))
                  (xfile (teximg-string-to-tex string tfile))
                  (xfile (teximg-tex-to-dvi xfile))
                  (xfile (teximg-dvi-to-png xfile scale fg bg)))
             (rename-file xfile ofile t)
             (unless (file-newer-than-file-p tfile ofile)
               (create-image ofile 'png nil :ascent 'center))))
          (t (create-image ofile 'png nil :ascent 'center)))))


;;; Asynchronous
(defun teximg-a-tex-to-dvi (ifile &optional cont)
  (let* ((ofile (concat (file-name-sans-extension ifile) ".dvi"))
         (proc (start-process "latex" nil "latex" ifile))
         (sent (lambda (p s)
                 (when (and cont p (string= s "finished\n"))
                   (funcall cont ofile)))))
    (set-process-sentinel proc sent)))

(defun teximg-a-dvi-to-png (ifile dpi fg bg &optional cont)
  (let* ((ofile (concat (file-name-sans-extension ifile) ".png"))
         (proc (start-process "dvipng" nil "dvipng"
                              "-fg" fg "-bg" bg
                              "-D" (number-to-string dpi)
                              "-T" "tight" "--gamma" "4" "-o" ofile
                              ifile))
         (sent (lambda (p s)
                 (when (and cont p (string= s "finished\n"))
                   (funcall cont ofile)))))
    (set-process-sentinel proc sent)))

(defun teximg-a-compile (string &optional scale foreground background)
  "Return filename of image produced by compiling STRING as LaTeX snippet."
  (let* ((default-directory *teximg-scratch*)
         (tfile (expand-file-name (concat (sha1 string) ".tex")))
         (ofile (expand-file-name (concat (sha1 string) ".png"))))
    (cond ((or scale (not (file-exists-p ofile)))
           (clear-image-cache ofile)
           (let* ((scale (* (or scale 1.0) ;(face-attribute 'default :height)
                            7.227 (aref (font-info (font-get-system-font)) 2)))
                  (fg (teximg-dvi-color (or foreground teximg-foreground)))
                  (bg (teximg-dvi-color (or background teximg-background)))
                  (xfile (teximg-string-to-tex string tfile)))
             (teximg-a-tex-to-dvi
              xfile (lambda (x) (teximg-a-dvi-to-png x scale fg bg)))
             (create-image ofile 'png nil :ascent 'center)))
          (t (create-image ofile 'png nil :ascent 'center)))))


(defun teximg-retrieve (string &optional scale fg bg)
  ;; "http://thewe.net/tex/"
  ;; ".nyud.net"
  ;; "http://www.codecogs.com" cacheServer "/gif.latex?"
  ;; "http://chart.apis.google.com" cacheServer "/chart?cht=tx&chl="
  ;; escape(expr).replace(/\+/g,"%2B")
  (let* ((default-directory *teximg-scratch*)
         (ofile (expand-file-name (concat (sha1 string) ".png")))
         (fg (or fg (apply #'color-rgb-to-hex
                           (color-name-to-rgb
                            (frame-parameter nil 'foreground-color)))))
         (bg (or bg (apply #'color-rgb-to-hex
                           (color-name-to-rgb
                            (frame-parameter nil 'background-color)))))
         (url (concat "http://chart.apis.google.com/"
                      "chart?cht=tx&chco=" (substring fg 1)
                      "&chf=bg,s,FFFFFF40"
                      "&chl=" (url-hexify-string string))))
    (ignore bg)
    (when (or scale (not (file-exists-p ofile)))
      (clear-image-cache ofile)
      (url-copy-file url ofile t))
    (create-image ofile 'png nil :ascent 'center)))

(defun teximg-clear-cache ()
  (interactive)
  (let ((default-directory *teximg-scratch*))
    (loop for x in (directory-files default-directory)
          when (string= "png" (file-name-extension x))
          do (delete-file x)))
  (clear-image-cache))

(defun teximg-region (start end)
  (interactive "r")
  (let ((o (find 'teximg (overlays-in start end)
                 :key (lambda (x) (overlay-get x 'evaporate)))))
    (if o (delete-overlay o)
      (let ((o (make-overlay start end))
            (i (teximg-compile (buffer-substring-no-properties start end))))
        (overlay-put o 'display i)
        (overlay-put o 'evaporate 'teximg)))))


;;;###autoload
(defun teximg (string &optional scale foreground background)
  (teximg-compile string scale foreground background))

(eval-when (load)
  (fset 'teximg
        (cond ((and (executable-find "latex")
                    (executable-find "dvipng"))
               'teximg-compile)
              ('teximg-retrieve))))


(provide 'teximg)

