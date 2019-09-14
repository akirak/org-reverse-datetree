;;; org-reverse-datetree.el --- Create reverse date trees in org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.3
;; Package-Requires: ((emacs "26.1") (dash "2.12"))
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
;; in a reversed order.  This is convenient in situation where
;; you want to find the latest status of a particular subject
;; using a search tool like `helm-org-rifle'.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)
(require 'dash)

(autoload 'org-element-map "org-element")
(autoload 'org-element-parse-buffer "org-element")
(autoload 'org-element-property "org-element")
(defvar org-agenda-buffer-name)
(defvar org-agenda-bulk-marked-entries)
(defvar org-agenda-persistent-marks)
(autoload 'org-agenda-error "org-agenda")
(autoload 'org-agenda-bulk-unmark-all "org-agenda")
(autoload 'org-agenda-redo "org-agenda")
(autoload 'org-remove-subtree-entries-from-agenda "org-agenda")

(defgroup org-reverse-datetree nil
  "Reverse date trees for Org mode."
  :group 'org
  :prefix "org-reverse-datetree-")

(defcustom org-reverse-datetree-year-format "%Y"
  "Year format used by org-reverse-datetree."
  :type '(choice string function)
  :group 'org-reverse-datetree)

(defcustom org-reverse-datetree-month-format "%Y-%m %B"
  "Month format used by org-reverse-datetree."
  :type '(choice string function)
  :group 'org-reverse-datetree)

(defcustom org-reverse-datetree-week-format "%Y W%W"
  "Week format used by org-reverse-datetree.

%U is the week number starting on Sunday and %W starting on Monday."
  :type '(choice string function)
  :group 'org-reverse-datetree)

(defcustom org-reverse-datetree-date-format "%Y-%m-%d %A"
  "Date format used by org-reverse-datetree."
  :type '(choice string function)
  :group 'org-reverse-datetree)

(defcustom org-reverse-datetree-find-function
  'org-reverse-datetree--find-or-insert
  "Function used to find a location of a date tree or insert it."
  :type '(choice (symbol org-reverse-datetree--find-or-insert)
                 (symbol org-reverse-datetree--find-or-prepend))
  :group 'org-reverse-datetree)

(defcustom org-reverse-datetree-level-formats nil
  "List of formats for date trees.

This setting affects the behavior of
`org-reverse-datetree-goto-date-in-file' and
`org-reverse-datetree-goto-read-date-in-file'.

Each item in this variable corresponds to each level in date
trees.  Note that this variable is buffer-local, so you can also
set it either as a file-local variable or as a directory-local
variable.

If this variable is non-nil, it take precedence over the settings
in the Org header."
  :type '(repeat (choice string
                         function))
  :group 'org-reverse-datetree
  :safe nil)

(make-variable-buffer-local 'org-reverse-datetree-level-formats)

(defvar-local org-reverse-datetree--file-headers nil
  "Alist of headers of the buffer.")

(cl-defun org-reverse-datetree--find-or-prepend (level text
                                                       &key append-newline)
  "Find or create a heading at a given LEVEL with TEXT.

If APPEND-NEWLINE is non-nil, a newline is appended to the
inserted text.

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
      (insert (concat "\n" prefix text
                      (when append-newline
                        "\n")))
      text)))

(defun org-reverse-datetree--apply-format (format time)
  "Apply date FORMAT to TIME to produce a string.

The format can be either a function or a string."
  (cl-etypecase format
    (string (format-time-string format time))
    (function (funcall format time))))

;;;###autoload
(defun org-reverse-datetree-2 (time level-formats return-type)
  "Jump to the specified date in a reverse date tree.

TIME is the date to be inserted.  If omitted, it will be today.

LEVEL-FORMATS is a list of formats.
See `org-reverse-datetree-level-formats' for the data type.

Depending on the value of RETURN-TYPE, this function returns the
following values:

\"'marker\":
  Returns the marker of the subtree.

\"point\"
  Returns point of subtree.

\"rfloc\"
  Returns a refile location spec that can be used as the third
  argument of `org-refile' function.

\"created\"
  Returns non-nil if and only if a new tree is created."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (save-restriction
    (widen)
    (org-save-outline-visibility nil
      (outline-show-all)
      (goto-char (point-min))
      (cl-loop for (level . format) in (-zip (number-sequence 1 (length level-formats))
                                             (-butlast level-formats))
               do (funcall org-reverse-datetree-find-function
                           level
                           (org-reverse-datetree--apply-format format time)))
      (let ((new (funcall org-reverse-datetree-find-function (length level-formats)
                          (org-reverse-datetree--apply-format (-last-item level-formats) time))))
        (cl-case return-type
          ('marker (point-marker))
          ('point (point))
          ('rfloc (list (nth 4 (org-heading-components))
                        (buffer-file-name (or (org-base-buffer (current-buffer))
                                              (current-buffer)))
                        nil
                        (point)))
          ('created new))))))

;;;###autoload
(cl-defun org-reverse-datetree-1 (&optional time
                                            &key
                                            week-tree
                                            return)
  "Jump to the specified date in a reverse date tree.

This function is deprecated.
Use `org-reverse-datetree-2' instead.

A reverse date tree is a reversed version of the date tree in
`org-capture', i.e. a date tree where the newest date is the first.
This is especially useful for a notes archive, because the latest
entry on a particular topic is displayed at the top in
a command like `helm-org-rifle'.

`org-reverse-datetree-find-function' is used to find or insert trees.

TIME is the date to be inserted. If omitted, it will be today.

If WEEK-TREE is non-nil, it creates week trees.  Otherwise, it
creates month trees.

For RETURN, see the documentation of `org-reverse-datetree-2'."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (let ((time (or time (current-time)))
        (level-formats (org-reverse-datetree--level-formats
                        (if week-tree
                            'week
                          'month))))
    (org-reverse-datetree-2 time level-formats return)))

(make-obsolete 'org-reverse-datetree-1 'org-reverse-datetree-2 "0.3.0")

(defun org-reverse-datetree--level-formats (tree-type)
  "Build `org-reverse-datetree-level-formats' for TREE-TYPE."
  (cl-ecase tree-type
    (month
     (list org-reverse-datetree-year-format
           org-reverse-datetree-month-format
           org-reverse-datetree-date-format))
    (week
     (list org-reverse-datetree-year-format
           org-reverse-datetree-week-format
           org-reverse-datetree-date-format))
    (month-and-week
     (list org-reverse-datetree-year-format
           org-reverse-datetree-month-format
           org-reverse-datetree-week-format
           org-reverse-datetree-date-format))))

(cl-defun org-reverse-datetree--find-or-insert (level text)
  "Find or create a heading with the given text at the given level.

LEVEL is the level of a tree, and TEXT is a heading of the tree.

This function uses string comparison to compare the dates in two
trees.  Therefore your date format must be alphabetically ordered,
e.g. beginning with YYYY(-MM(-DD)).

If a new tree is created, non-nil is returned."
  (declare (indent 1))
  (let* ((prefix (concat (make-string (org-get-valid-level level) ?*) " "))
         (bounds (delq nil (list (save-excursion
                                   (when (re-search-forward
                                          (rx bol "# Local Variables:")
                                          nil t)
                                     (line-end-position)))
                                 (unless (= level 1)
                                   (save-excursion
                                     (org-end-of-subtree))))))
         (bound (when bounds (-min bounds)))
         created
         found)
    (catch 'search
      (while (and (or (not bound)
                      (> bound (point)))
                  (re-search-forward (concat "^" (regexp-quote prefix))
                                     bound t))
        (let ((here (nth 4 (org-heading-components))))
          (cond
           ((string-equal here text) (progn
                                       (end-of-line 1)
                                       (setq found t)
                                       (throw 'search t)))
           ((string< here text) (progn
                                  (end-of-line 0)
                                  (org-reverse-datetree--insert-heading
                                   prefix text)
                                  (setq created t
                                        found t)
                                  (throw 'search t)))))))
    (unless found
      (goto-char (or bound (point-max)))
      (org-reverse-datetree--insert-heading
       prefix text)
      (setq created t))
    created))

(defun org-reverse-datetree--insert-heading (prefix text)
  "Insert a heading at a particular level into the point.

This function inserts a heading smartly depending on empty lines
around the point.

PREFIX is a prefix of the heading which consists of one or more
asterisks and a space.

TEXT is a heading text."
  ;; If the point is not at bol
  (unless (looking-at (rx bol))
    ;; If there is a blank line after the point
    (if (looking-at (rx (>= 2 "\n")))
        ;; Go to the bol immediately after the point
        (forward-char 1)
      ;; Ensure a new line is inserted
      (insert "\n")))
  (insert (if (org--blank-before-heading-p)
              "\n"
            "")
          prefix text))

;;;; Retrieving configuration from the file header

(defun org-reverse-datetree--get-file-headers ()
  "Get the file headers of the current Org buffer."
  (let ((buffer-ast (org-with-wide-buffer (org-element-parse-buffer))))
    (setq org-reverse-datetree--file-headers
          (org-element-map buffer-ast 'keyword
            (lambda (keyword)
              (cons (org-element-property :key keyword)
                    (org-element-property :value keyword)))))))

(defun org-reverse-datetree--insert-header (key value)
  "Insert a pair of KEY and VALUE into the file header."
  (org-with-wide-buffer
   (goto-char (point-min))
   (if (re-search-forward (concat (rx bol "#+")
                                  (regexp-quote key)
                                  (rx ":" (1+ space)))
                          (save-excursion
                            (re-search-forward (rx bol "*") nil t)
                            (point))
                          t)
       (progn
         (kill-line)
         (insert value))
     (when (string-prefix-p "#" (thing-at-point 'line))
       (forward-line))
     (insert "#+" key ": " value "\n")
     ;; Update the cached value stored as a buffer-local variable
     (let ((pair (assoc key org-reverse-datetree--file-headers)))
       (if pair
           (setcdr pair value)
         (push (cons key value) org-reverse-datetree--file-headers))))))

(defun org-reverse-datetree--lookup-header (key)
  "Look up KEY from the file headers stored as a local variable."
  (cdr (assoc key org-reverse-datetree--file-headers)))

(defun org-reverse-datetree--lookup-type-header-1 ()
  "Look up a boolean file header or ask for a value.

This function looks up KEY from the file headers.  If the key is
not contained, it asks for a new value with PROMPT, inserts the value
into the header, and returns the value."
  (let ((header "REVERSE_DATETREE_USE_WEEK_TREE"))
    (pcase (org-reverse-datetree--lookup-header header)
      ("month-and-week" 'month-and-week)
      ("t" 'week)
      ("nil" 'month)
      ('nil (let* ((char (read-char-choice "Choose a datetree type ([y/w] week, [n/m] month, [b] week and month): "
                                           (string-to-list "ywnmb")))
                   (value (cl-case char
                            ((?y ?w) 'week)
                            ((?n ?m) 'month)
                            ((?b) 'month-and-week))))
              (org-reverse-datetree--insert-header header
                                                   (cl-case value
                                                     (week "t")
                                                     (month "nil")
                                                     (month-and-week "month-and-week")))
              value)))))

(defun org-reverse-datetree--parse-format (raw)
  "Parse a RAW time format string in the header.

If the first character of the string is either a single quotation
or an open parenthesis, it is read as a function.  Otherwise, it
is a string passed to `format-time-string' as the first argument."
  (unless (stringp raw)
    (user-error "Must be a string: %s" raw))
  (cond
   ((string-match (rx bol (any "'(")) raw)
    (read raw))
   (t
    raw)))

(defun org-reverse-datetree--lookup-format-header (key prompt initial)
  "Look up a string file header or ask for a value.

This function looks up KEY from the file headers.  If the key is
not contained, it asks for a new value with PROMPT with INITIAL
as the default value, inserts the value, and returns the value."
  (if-let ((value (org-reverse-datetree--lookup-header key)))
      (org-reverse-datetree--parse-format (string-trim value))
    (let* ((raw (read-string prompt initial))
           (ret (org-reverse-datetree--parse-format raw)))
      (org-reverse-datetree--insert-header key raw)
      ret)))

(defun org-reverse-datetree--lookup-string-header (key prompt initial)
  "Look up a string file header or ask for a value.

This function looks up KEY from the file headers.  If the key is
not contained, it asks for a new value with PROMPT with INITIAL
as the default value, inserts the value, and returns the value."
  (if-let ((value (org-reverse-datetree--lookup-header key)))
      (string-trim value)
    (let ((ret (read-string prompt initial)))
      (org-reverse-datetree--insert-header key ret)
      ret)))

;;;###autoload
(cl-defun org-reverse-datetree-goto-date-in-file (&optional time
                                                            &key return)
  "Find or create a heading as configured in the file headers.

This function finds an entry at TIME in a date tree as configured
by file headers of the buffer.  If there is no such configuration,
ask the user for a new configuration.  If TIME is omitted, it is
the current date.  RETURN is the same as in `org-reverse-datetree-1'.

When this function is called interactively, it asks for TIME using
`org-read-date' and go to an entry of the date."
  (interactive (list (org-read-date nil t nil)
                     :return nil))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (if org-reverse-datetree-level-formats
      (org-reverse-datetree-2 time org-reverse-datetree-level-formats
                              return)
    (org-reverse-datetree--get-file-headers)
    (let* ((type (org-reverse-datetree--lookup-type-header-1))
           (org-reverse-datetree-year-format
            (org-reverse-datetree--lookup-format-header
             "REVERSE_DATETREE_YEAR_FORMAT"
             "Year format: "
             org-reverse-datetree-year-format))
           (org-reverse-datetree-month-format
            (when (memq type '(month month-and-week))
              (org-reverse-datetree--lookup-format-header
               "REVERSE_DATETREE_MONTH_FORMAT"
               "Month format: "
               org-reverse-datetree-month-format)))
           (org-reverse-datetree-week-format
            (when (memq type '(week month-and-week))
              (org-reverse-datetree--lookup-format-header
               "REVERSE_DATETREE_WEEK_FORMAT"
               "Week format: "
               org-reverse-datetree-week-format)))
           (org-reverse-datetree-date-format
            (org-reverse-datetree--lookup-format-header
             "REVERSE_DATETREE_DATE_FORMAT"
             "Date format: "
             org-reverse-datetree-date-format))
           (org-reverse-datetree-level-formats
            (org-reverse-datetree--level-formats type)))
      (org-reverse-datetree-2 time org-reverse-datetree-level-formats
                              return))))

(cl-defun org-reverse-datetree-goto-read-date-in-file (&rest args)
  "Find or create a heading as configured in the file headers.

This function is like `org-reverse-datetree-goto-date-in-file',
but it always asks for a date even if it is called non-interactively."
  (interactive)
  (apply #'org-reverse-datetree-goto-date-in-file
         (org-read-date nil t nil)
         (cdr args)))

(defun org-reverse-datetree--timestamp-to-time (s)
  "Convert timestamp string S into internal time."
  (apply #'encode-time (org-parse-time-string s)))

(defun org-reverse-datetree--timestamp-from-string (s)
  "Convert Org timestamp S, as a string, into a timestamp object.
Return nil if S is not a valid timestamp string."
  (when (org-string-nw-p s)
    (with-temp-buffer
      (save-excursion (insert s))
      (org-element-timestamp-parser))))

(defun org-reverse-datetree--parse-timestamp-string (s)
  "Parse a timestamp string S and return a corresponding Emacs time."
  (org-reverse-datetree--timestamp-to-time
   (org-reverse-datetree--timestamp-from-string s)))

(cl-defun org-reverse-datetree--get-entry-time (&key ask-always
                                                     (prefer '("CLOSED")))
  "Get an Emacs time for the current Org entry.

This function retrieves a timestamp from a property of the entry.
By default, it checks for the closed date of the entry.
If there is no value set as the property, it asks for a date using
`org-read-date' unless if ASK-ALWAYS is non-nil.

You can specify properties to retrieve a timestamp from by
setting PREFER.  It can be a string or a list of strings.
If it is a list, the first existing property property is used.

This function returns an Emacs time."
  (let ((attempt (-some (lambda (property)
                          (org-entry-get nil property))
                        (cl-typecase prefer
                          (list prefer)
                          (t (list prefer))))))
    (cond
     (ask-always
      (org-read-date nil t nil nil
                     (org-reverse-datetree--timestamp-to-time attempt)))
     (attempt
      (org-reverse-datetree--timestamp-to-time attempt))
     (t (org-read-date nil t nil nil)))))

(cl-defun org-reverse-datetree--refile-to-file (file &optional time
                                                     &key ask-always prefer)
  "Refile the current single Org entry.

This is used inside `org-reverse-datetree-refile-to-file' to
refile a single tree.
FILE, TIME, ASK-ALWAYS, and PREFER are the same as in the function.

Return a string describing the operation."
  (let* ((time (or time
                   (org-reverse-datetree--get-entry-time
                    :ask-always ask-always
                    :prefer prefer)))
         (rfloc (with-current-buffer
                    (or (find-buffer-visiting file)
                        (find-file-noselect file))
                  (save-excursion
                    (org-reverse-datetree-goto-date-in-file
                     time :return 'rfloc))))
         (heading (nth 4 (org-heading-components))))
    (org-refile nil nil rfloc)
    (format "\"%s\" -> %s" heading (car rfloc))))

;;;###autoload
(cl-defun org-reverse-datetree-refile-to-file (file &optional time
                                                    &key ask-always prefer)
  "Refile the current Org entry into a configured date tree in a file.

This function refiles the current entry into a date tree in FILE
configured in the headers of the file. The same configuration as
`org-reverse-datetree-goto-date-in-file' is used.

The location in the date tree is specified by TIME, which is an
Emacs time.  If TIME is not set, a timestamp is retrieved from
properties of the current entry using
`org-reverse-datetree--get-entry-time' with ASK-ALWAYS and PREFER
as arguments."
  ;; NOTE: Based on org 9.3. Maybe needs updating in the future
  (pcase (derived-mode-p 'org-mode 'org-agenda-mode)
    ('org-mode
     (if (org-region-active-p)
         (let ((region-start (region-beginning))
               (region-end (region-end))
               (org-refile-active-region-within-subtree nil)
               subtree-end
               msgs)
           (org-with-wide-buffer
            (when (file-equal-p file (buffer-file-name))
              (user-error "Can't refile to the same file"))
            (deactivate-mark)
            (goto-char region-start)
            (org-back-to-heading)
            (setq region-start (point))
            (while region-start
              (unless (org-at-heading-p)
                (org-next-visible-heading 1))
              (setq subtree-end (save-excursion
                                  (org-end-of-subtree)))
              (push (org-reverse-datetree--refile-to-file
                     file time :ask-always ask-always :prefer prefer)
                    msgs)
              (if (<= region-end subtree-end)
                  (setq region-start nil)
                (let ((len (- subtree-end region-start)))
                  (setq region-end (- region-end len))
                  (goto-char region-start)))))
           (let ((message-log-max nil))
             (message (format "Refiled to %s:\n%s"
                              file
                              (string-join (nreverse msgs) "\n")))))
       (org-reverse-datetree--refile-to-file
        file time :ask-always ask-always :prefer prefer)))
    ('org-agenda-mode
     (if org-agenda-bulk-marked-entries
         (dolist (group (seq-group-by #'marker-buffer
                                      org-agenda-bulk-marked-entries))
           (let ((processed 0)
                 (skipped 0)
                 (d 0)
                 msgs)
             (dolist (e (cl-sort (cdr group) (lambda (a b)
                                               (< (marker-position a)
                                                  (marker-position b)))))
               (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e))
                     org-loop-over-headlines-in-active-region)
                 (if (not pos)
                     (progn
                       (message "Skipped removed entry")
                       (cl-incf skipped))
                   (goto-char pos)
                   (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                                      (org-agenda-error))))
                     (with-current-buffer (marker-buffer marker)
                       (org-with-wide-buffer
                        (goto-char (- (marker-position marker) d))
                        (setq d (+ d (- (save-excursion
                                          (org-end-of-subtree))
                                        (point))))
                        (push (org-reverse-datetree--refile-to-file
                               file time :ask-always ask-always :prefer prefer)
                              msgs))))
                   ;; `post-command-hook' is not run yet.  We make sure any
                   ;; pending log note is processed.
                   (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                             (memq 'org-add-log-note post-command-hook))
                     (org-add-log-note))
                   (cl-incf processed))))
             (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
             (org-agenda-redo)
             (message "Refiled %d entries to %s%s\n%s"
                      processed file
                      (if (= skipped 0)
                          ""
                        (format ", skipped %d" skipped))
                      (string-join (nreverse msgs) "\n"))))
       (let* ((buffer-orig (buffer-name))
              (marker (or (org-get-at-bol 'org-hd-marker)
                          (org-agenda-error))))
         (with-current-buffer (marker-buffer marker)
           (org-with-wide-buffer
            (goto-char (marker-position marker))
            (let* (lexical-binding
                   (org-agenda-buffer-name buffer-orig))
              (org-remove-subtree-entries-from-agenda))
            (org-reverse-datetree--refile-to-file
             file time :ask-always ask-always :prefer prefer))))))
    (_ (user-error "Not in org-mode or org-agenda-mode"))))

;;;; Utility functions for defining formats
(defun org-reverse-datetree-monday (&optional time)
  "Get Monday in the same week as TIME."
  (org-reverse-datetree-last-dow 1 time))

(defun org-reverse-datetree-sunday (&optional time)
  "Get Sunday in the same week as TIME."
  (org-reverse-datetree-last-dow 0 time))

(defun org-reverse-datetree-last-dow (n &optional time)
  "Get the date on N th day of week in the same week as TIME."
  (let* ((time (or time (current-time)))
         (x (- (org-reverse-datetree--dow time) n)))
    (time-add time (- (* 86400 (if (>= x 0) x (+ x 7)))))))

(defun org-reverse-datetree--dow (time)
  "Get the day of week of TIME."
  (nth 6 (decode-time time)))

;;;; Maintenance commands

;;;###autoload
(defun org-reverse-datetree-cleanup-empty-dates ()
  "Delete empty date entries in the buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (let ((search-spaces-regexp (rx (+ (any " \t\r\n")))))
    (when (yes-or-no-p "Start from the beginning?")
      (goto-char (point-min)))
    (while (re-search-forward (rx (group bol "*** " (* nonl) (* space) "\n")
                                  "*** ")
                              nil t)
      (goto-char (match-beginning 1))
      (push-mark (match-end 1))
      (setq mark-active t)
      (when (yes-or-no-p "Delete this empty date?")
        (call-interactively #'delete-region)))
    (when (yes-or-no-p "Delete empty week/month entries from the beginning as well?")
      (goto-char (point-min))
      (while (re-search-forward (rx (group bol "** " (* nonl) (* space) "\n")
                                    "** ")
                                nil t)
        (goto-char (match-beginning 1))
        (push-mark (match-end 1))
        (setq mark-active t)
        (when (yes-or-no-p "Delete this empty entry?")
          (call-interactively #'delete-region))))))

(provide 'org-reverse-datetree)
;;; org-reverse-datetree.el ends here
