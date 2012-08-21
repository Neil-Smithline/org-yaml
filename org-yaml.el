;;; org-yaml.el --- Export `org-mode' entries as YAMLized `org-mode' entries
;; Author: Neil Smithline
;; Maintainer:
;; Copyright (C) 2012, Neil Smithline, all rights reserved.
;; Created: Tue Aug 21 11:35:45 2012 (-0400)
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; NOT READY FOR PRIME TIME!!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:



(defun xx ()
  (interactive)
  (org-convert-to-yaml "+tumblr=\"ready\"" '("tumblr")))

(defun org-convert-to-yaml (search-string custom-ignored-props)
  (interactive)
  (let ((counter 0)
        (created-buffers))
    (org-map-entries #'(lambda ()
                         (push (format "*YAML-%s*" (incf counter)) created-buffers)
			 (org-convert-to-yaml-helper (car created-buffers)
						     custom-ignored-props))
                     search-string
                     'file
                     'comment)
    (message "buffs=%s." created-buffers)))

(defconst org-convert-to-yaml-unwanted-props
  '("FILE" "TAGS" "ALLTAGS" "BLOCKED" "CATEGORY"))

(defun org-convert-to-yaml-properties-cleanup (props custom-ignored-props)
  "Return PROPS alist with undesirable keys removed.
The keys in `org-convert-to-yaml-unwanted-props' and CUSTOM-IGNORED-PROPS will be excluded in the returned alist."
  (let ((rtn))
    (mapc #'(lambda (prop)
	      (unless (or (member (car prop) org-convert-to-yaml-unwanted-props)
			  (member (car prop) custom-ignored-props))
		(setq rtn (cons prop rtn))))
	  props)
    rtn))

(defun org-convert-to-yaml-helper (buffer custom-ignored-props &optional append)
  "Put YAML for the current entry in BUFFER.
When optional APPEND, append the YAML to BUFFER rather replacing
its contents with the YAML."
  (let* ((start         (point))
         (prop-block    (org-get-property-block))
         (filename      (buffer-file-name))
         (buffername    (buffer-name)))
    (when prop-block
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                           WARNING
      ;;
      ;; The ordering of the assignments in the following `let*'
      ;; declaration is important. Some of the assignments depend on
      ;; point being in a specific location relative to the header we
      ;; are exporting. Other assignments move point around.
      ;; Rearranging these could be bad.
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (let* ((props-start   (car prop-block))
             (props-end     (cdr prop-block))
	     (props	    (org-convert-to-yaml-properties-cleanup
			     (org-entry-properties)
			     custom-ignored-props))
             (yaml-props    (mapconcat #'(lambda (p) (concat (car p) ": " (cdr p))) props "\n"))

             (heading-start (progn (org-back-to-heading) (point)))
             ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             ;; FIXME
             ;;
             ;; body-end is broken because it only looks for siblings
             ;; or EOB. It should stop at the minimum of next sibling,
             ;; EOB, next heading of a higher level.
             ;;
             ;; The best solution I have to find a heading of a higher
             ;; level is to walk up the current headline's parents
             ;; looking for a sibling. Once that sibling is found it
             ;; should be used if there is no next sibling.
             ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             (heading-end   (progn (end-of-line 1) (point)))
             (heading       (buffer-substring-no-properties heading-start heading-end))

             (id            (when filename (org-id-get-create)))
             (id-text       (org-get-heading))

             (body-start    (progn (org-end-of-meta-data-and-drawers) (point)))
             (body-end      (progn (goto-char start) (or (org-get-next-sibling) (point-max))))
             (body          (buffer-substring-no-properties body-start body-end))

             (new-heading))
        (setq org-map-continue-from (1- body-end))
        ;; (message "s=%s, e=%s." body-start body-end)
        (switch-to-buffer buffer)
        (toggle-read-only -1)
        (if append
            (goto-char (point-max))
          (delete-region (point-min) (point-max)))
        (insert "\n")
        (insert "#+BEGIN_HTML\n\n")
        (insert "---\n")
        (insert yaml-props)
        (insert "\n---\n")
        (insert "\n#+END_HTML\n\n")
        (insert "* COMMENT README\n")
        (insert "\n- This text has been automatically generated by `org-convert-to-yaml'.\n")
        (if filename
            (progn
              (insert (format "- The source file was [[file:%s]].\n" filename))
              (insert (format "- The source headline was [[id:%s][%s]].\n" id id-text)))
          (insert (format "- The source buffer was %s.\n" buffername))
          (insert (format "- The source headline was `%s'.\n" id-text)))
        (insert "\nAny changes you make to this text may be lost if `org-convert-to-yaml' is rerun.")
        (insert "\n\n\n\n\n")
        (setq new-heading (point))      ; Save start of our new heading
        (insert heading)
        (insert "\n")
        (insert body)
        (org-mode)
        ;; Go to new heading and promote it to level 1.
        (goto-char new-heading)
        (while (< 1 (org-current-level))
          (org-promote-subtree))))))

;; (org-entry-put (point) "foo" "goo")
 ;; (org-toggle-tag
(provide 'org-yaml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-yaml.el ends here
