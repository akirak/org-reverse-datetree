;;; org-reverse-datetree.el --- Create reverse date trees in org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1"))
;; Keywords: outlines
;; URL: https://github.com/akirak/org-reverse-datetree

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a function for creating reverse date trees,
;; which is similar to date trees supported by `org-capture' but
;; in a reversed order. This is convenient in situation where
;; you want to find the latest status of a particular subject
;; using a search tool like `helm-org-rifle'.

;;; Code:

(defcustom org-reverse-datetree-year-format "%Y"
  "Year format used by org-reverse-datetree."
  :type 'string
  :group 'org-reverse-datetree)

(defcustom org-reverse-datetree-month-format "%Y-%m %B"
  "Month format used by org-reverse-datetree."
  :type 'string
  :group 'org-reverse-datetree)

(defcustom org-reverse-datetree-week-format "%Y W%W"
  "Week format used by org-reverse-datetree.

%U is the week number starting on Sunday and %W starting on Monday."
  :type 'string
  :group 'org-reverse-datetree)

(defcustom org-reverse-datetree-date-format "%Y-%m-%d %A"
  "Date format used by org-reverse-datetree."
  :type 'string
  :group 'org-reverse-datetree)

(defcustom org-reverse-datetree-find-function
  'org-reverse-datetree--find-or-insert
  "Function used to find a location of a date tree or insert it."
  :type '(choice (symbol org-reverse-datetree--find-or-insert)
                 (symbol org-reverse-datetree--find-or-prepend))
  :group 'org-reverse-datetree)

(defun org-reverse-datetree--find-or-prepend (level text)
  "Find or create a heading with the given text at the given level.

If a new tree is created, non-nil is returned."
  (declare (indent 1))
  (let ((prefix (concat (make-string (org-get-valid-level level) ?*) " "))
        (bound (unless (= level 1)
                 (save-excursion (org-end-of-subtree)))))
    (unless (re-search-forward (concat "^" (regexp-quote prefix) text)
                               bound t)
      (if (re-search-forward (concat "^" prefix) bound t)
          (end-of-line 0)
        (end-of-line 1))
      (insert "\n" prefix text)
      text)))

;;;###autoload
(cl-defun org-reverse-datetree-1 (&optional time
                                            &key week-tree)
  "Jump to the specified date in a reverse date tree.

A reverse date tree is a reversed version of the date tree in
`org-capture', i.e. a date tree where the newest date is the first.
This is especially useful for a notes archive, because the latest
entry on a particular topic is displayed at the top in
a command like `helm-org-rifle'.

`org-reverse-datetree-find-function' is used to find or insert trees.

TIME is the date to be inserted. If omitted, it will be today.

If WEEK-TREE is non-nil, create a week tree.

If a new tree is created, non-nil is returned."
  (let* ((time (or time (current-time))))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (funcall org-reverse-datetree-find-function 1
               (format-time-string org-reverse-datetree-year-format time))
      (funcall org-reverse-datetree-find-function 2
               (format-time-string (if week-tree
                                       org-reverse-datetree-week-format
                                     org-reverse-datetree-month-format)
                                   time))
      (funcall org-reverse-datetree-find-function 3
               (format-time-string org-reverse-datetree-date-format time)))))

(defun org-reverse-datetree--find-or-insert (level text)
  "Find or create a heading with the given text at the given level.

LEVEL is the level of a tree, and TEXT is a heading of the tree.

This function uses string comparison to compare the dates in two
trees. Therefore your date format must be alphabetically ordered,
e.g. beginning with YYYY(-MM(-DD)).

If a new tree is created, non-nil is returned."
  (declare (indent 1))
  (let ((prefix (concat (make-string (org-get-valid-level level) ?*) " "))
        (bound (unless (= level 1)
                 (save-excursion (org-end-of-subtree))))
        created
        found)
    (catch 'search
      (while (re-search-forward (concat "^" (regexp-quote prefix))
                                bound t)
        (let ((here (nth 4 (org-heading-components))))
          (cond
           ((string-equal here text) (progn
                                       (end-of-line 1)
                                       (setq found t)
                                       (throw 'search t)))
           ((string< here text) (progn
                                  (end-of-line 0)
                                  (insert "\n" prefix text)
                                  (setq created t
                                        found t)
                                  (throw 'search t)))))))
    (unless found
      (goto-char (or bound (point-max)))
      (insert "\n" prefix text)
      (setq created t))
    created))

(provide 'org-reverse-datetree)
;;; org-reverse-datetree.el ends here
