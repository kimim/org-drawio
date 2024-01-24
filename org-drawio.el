;;; org-drawio.el --- Convert and include drawio image to orgmode

;; Copyright (c) 2024 Kimi Ma <kimi.im@outlook.com>

;; Author:  Kimi Ma <kimi.im@outlook.com>
;; URL: https://github.com/kimim/org-drawio
;; Keywords: multimedia convenience
;; Version: 0.1
;; Package-Requires: ((org "9.6.6") (emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Acknowledgements:

;; Inspired a lot by https://github.com/nobiot/org-transclusion from
;; Noboru Ota <me@nobiot.com>

;;; Commentary:

;; When you are drawing with draw.io and you want to insert the svg
;; image exported from drawio file, the operation is nontrivial.  This
;; extension provide two commands to reduce the manual work:
;;
;; - `org-drawio-add' convert drawio to svg, and insert it to org
;; - `org-drawio-open' open drawio file at the point

;;; Installation

;; To enable, add the following:
;;
;;   (use-package org-drawio
;;     :custom ((org-drawio-input-dir "./draws")
;;              (org-drawio-output-dir "./images")
;;              (org-drawio-output-crop t) ;; crop the image
;;              (org-drawio-output-page "0")))
;;
;; You may need to install drawio and pdf2svg in your PATH.

;;; Customization

;; `org-drawio-command-drawio': customzie drawio cli path
;; `org-drawio-command-pdf2svg': customize pdf2svg cli path
;; `org-drawio-input-dir': drawio input folder, default is ./draws
;; `org-drawio-output-dir': default svg output folder, default is ./images
;; `org-drawio-page': page from drawio file, default is page 0
;; `org-drawio-crop': whether crop the image, default is nil
;; `org-drawio-dir-regex': customize folder regex
;; `org-drawio-file-regex': customize file name regex

;;; Change log

;; 2024/01/24
;;      * required that all parameters be double quoted. thanks to owensys
;;
;; 2024/01/22
;;      * convert to svg asynchronously, thanks to twiddling
;;
;; 2024/01/21
;;      * add `org-drawio-crop', suggested by zealotrush
;;      * add `org-drawio-command-drawio'
;;      * add `org-drawio-command-pdf2svg'

;;; Code:
(require 'org)
(require 's)

(defgroup org-drawio nil
  "Convert drawio to svg image."
  :group 'org
  :prefix "org-drawio-"
  :link '(url-link :tag "Github" "https://github.com/kimim/org-drawio")
  :package-version '("Org-drawio" . "1.0.0"))

(defcustom org-drawio-command-drawio nil
  "Define command line path for draw.io."
  :type 'string)

(defcustom org-drawio-command-pdf2svg nil
  "Define command line path for pdf2svg."
  :type 'string)

(defcustom org-drawio-input-dir "./draws"
  "Define input folder of drawio file."
  :type 'string)

(defcustom org-drawio-output-dir "./images"
  "Define output folder of svg image."
  :type 'string)

(defcustom org-drawio-page "0"
  "Define output folder of svg image."
  :type 'string)

(defcustom org-drawio-crop nil
  "Define crop of the exported image."
  :type 'boolean)

(defcustom org-drawio-dir-regex
  "\"\\([\u4e00-\u9fa5:~ \\/a-z_\\-\s0-9\\.]+\\)\""
  "Define regex of directry. Currently support Chinese charaters."
  :type 'string)

(defcustom org-drawio-file-regex
  "\"\\([\u4e00-\u9fa5:~ \\/a-z_\\-\s0-9\\.]+\\)\""
  "Define regex of file. Currently support Chinese charaters."
  :type 'string)

(defun org-drawio-keyword-value-input-dir (string)
  "Convert a keyword STRING to plist."
  (when (string-match (concat ":input-dir +"
                              org-drawio-dir-regex)
                      string)
    (list :input-dir
          (org-trim (org-strip-quotes (match-string 1 string))))))

(defun org-drawio-keyword-value-input (string)
 "Convert a keyword STRING to plist."
 (when (string-match (concat "^" org-drawio-file-regex)
                     string)
    (list :input
          (org-trim (org-strip-quotes (match-string 1 string))))))

(defun org-drawio-keyword-value-output-dir (string)
  "Convert a keyword STRING to plist."
  (when (string-match (concat ":output-dir +"
                              org-drawio-dir-regex)
                      string)
    (list :output-dir
          (org-trim (org-strip-quotes (match-string 1 string))))))

(defun org-drawio-keyword-value-output (string)
  "Convert a keyword STRING to plist."
  (when (string-match (concat ":output +" org-drawio-file-regex)
                      string)
    (list :output
          (org-trim (org-strip-quotes (match-string 1 string))))))

(defun org-drawio-keyword-value-page (string)
  "Convert a keyword STRING to plist."
  (when (string-match ":page +\\([0-9]*\\)[ \\t\\n]*" string)
    (list :page
          (org-strip-quotes (match-string 1 string)))))

(defvar org-drawio-keyword-value-functions
  '(org-drawio-keyword-value-page
    org-drawio-keyword-value-output
    org-drawio-keyword-value-output-dir
    org-drawio-keyword-value-input
    org-drawio-keyword-value-input-dir)
  "Define a list of functions used to parse a #+drawio keyword.")

(defun org-drawio-keyword-string-to-plist ()
  "Return the \"#+drawio:\" keyword's values if any at point."
  (save-excursion
    (beginning-of-line)
    (let ((plist))
      (when (string= "DRAWIO"
                     (org-element-property :key (org-element-at-point)))
        ;; #+drawio: keyword exists.
        ;; Further checking the value
        (when-let ((str (org-element-property :value (org-element-at-point))))
          (dolist (fn org-drawio-keyword-value-functions) plist
                  (setq plist (append plist (funcall fn str)))))
        plist))))

;;;###autoload
(defun org-drawio-add ()
  "Convert .drawio file to .svg file, and insert svg to orgmode."
  (interactive)
  (save-excursion
    (let* ((keyword-plist (org-drawio-keyword-string-to-plist))
           (dio-input-dir (or (plist-get keyword-plist :input-dir)
                              org-drawio-input-dir))
           (dio-input (file-name-with-extension
                       (plist-get keyword-plist :input) "drawio"))
           (dio-page (or (plist-get keyword-plist :page)
                         org-drawio-page))
           (dio-output-dir (or (plist-get keyword-plist :output-dir)
                               org-drawio-output-dir))
           (dio-output (plist-get keyword-plist :output))
           ;; if output file specified, use it, otherwise append page to it.
           (dio-output-svg (if dio-output
                               (file-name-with-extension dio-output "svg")
                             (file-name-with-extension
                              (concat (file-name-sans-extension dio-output)
                                      "-" dio-page)
                              "svg")))
           (dio-output-pdf (file-name-with-extension dio-output-svg "pdf"))
           ;; create output dir if non exist
           (_ (when (not (file-exists-p dio-output-dir))
                (make-directory dio-output-dir)))
           (script (format "%s -x %s%s/%s -p %s -o %s/%s >/dev/null 2>&1 && \
%s %s/%s %s/%s >/dev/null 2>&1"
                           (or org-drawio-command-drawio
                               "draw.io")
                           (if org-drawio-crop "--crop " "")
                           (shell-quote-argument dio-input-dir)
                           (shell-quote-argument dio-input) dio-page
                           (shell-quote-argument dio-output-dir)
                           (shell-quote-argument dio-output-pdf)
                           (or org-drawio-command-pdf2svg
                               "pdf2svg")
                           (shell-quote-argument dio-output-dir)
                           (shell-quote-argument dio-output-pdf)
                           (shell-quote-argument dio-output-dir)
                           (shell-quote-argument dio-output-svg))))
      ;; skip #+caption, #+name of image
      (if (org-next-line-empty-p)
          (progn (end-of-line) (insert-char ?\n))
        (while (string-prefix-p "#+" (org-current-line-string (next-line)))))
      ;; convert from drawio to svg asynchronously, thanks to twiddling
      (let ((process (start-process-shell-command "org-drawio" nil script)))
        (set-process-sentinel
         process `(lambda (process event)
                    (when (string-match-p "finished" event)
                      ;; trash pdf file
                      (delete-file ,(concat dio-output-dir "/" dio-output-pdf) t)
                      ;; refresh image
                      (org-redisplay-inline-images)))))
      (when (string-prefix-p "[[" (org-current-line-string))
        ;; when it is image link
        (kill-whole-line 0))
      (insert (concat "[[file:" dio-output-dir "/" dio-output-svg "]]")))))

;;;###autoload
(defun org-drawio-open ()
  "Open .drawio file in current line of #+drawio keyword."
  (interactive)
  (let* ((keyword-plist (org-drawio-keyword-string-to-plist))
         (dio-input (file-name-with-extension
                     (plist-get keyword-plist :input) "drawio"))
         (dio-input-dir (or (plist-get keyword-plist :input-dir)
                            org-drawio-input-dir))
         (path (shell-quote-argument
                (concat dio-input-dir "/" dio-input))))
    (cond
     ;; ensure that draw.io.exe is in execute PATH
     ((string-equal system-type "windows-nt")
      (w32-shell-execute "open" (if org-drawio-command-drawio
                                    org-drawio-command-drawio
                                  "draw.io.exe")
                         path))
     ;; TODO: need some test for other systems
     ((string-equal system-type "darwin")
      (shell-command (concat (or org-drawio-command-drawio
                                 "draw.io")
                             (format " \"%s\"" path))))
     ((string-equal system-type "gnu/linux")
      (start-process "" nil "xdg-open"
                     (or org-drawio-command-drawio
                         "draw.io")
                     path))
     ((string-equal system-type "cygwin")
      (start-process "" nil "xdg-open" (or org-drawio-command-drawio
                                           "draw.io")
                     path)))))

(provide 'org-drawio)
;;; org-drawio.el ends here
