;;; org-drawio-test.el --- Tests for org-drawio -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org-drawio)

(ert-deftest org-drawio-open-launches-configured-command-on-linux ()
  "Open a Draw.io file with the configured executable on GNU/Linux."
  (let ((system-type 'gnu/linux)
        (org-drawio-command-drawio "drawio")
        (org-drawio-input-dir "./draws")
        start-process-arguments)
    (with-temp-buffer
      (org-mode)
      (insert "#+drawio: \"diagram\"\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'org-drawio-new-if-not-exist)
                 (lambda (&rest _)))
                ((symbol-function 'start-process)
                 (lambda (&rest arguments)
                   (setq start-process-arguments arguments))))
        (org-drawio-open)))
    (should (equal start-process-arguments
                   '("org-drawio" nil "drawio"
                     "./draws/diagram.drawio")))))

;;; org-drawio-test.el ends here
