;;; -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'org-reverse-datetree)

(defun org-reverse-datetree-test--collect-headings (level)
  (let (result)
    (goto-char (point-min))
    (while (re-search-forward (rx-to-string `(and bol
                                                  ,(make-string level ?*)
                                                  (+ space)))
                              nil t)
      (push (buffer-substring-no-properties (point) (line-end-position))
            result))
    (nreverse result)))

(describe "org-reverse-datetree-cleanup-empty-dates"
  (it "On custom-tree.org"
    (let ((headings (with-temp-buffer
                      (insert-file-contents "test/custom-tree.org")
                      (setq buffer-file-name "test/custom-tree.org")
                      (org-mode)
                      (setq-local org-reverse-datetree-level-formats
                                  '("%Y"
                                    (lambda (time)
                                      (format-time-string
                                       "%Y-%m %B"
                                       (org-reverse-datetree-monday time)))
                                    "%Y-%m-%d %A"))
                      (goto-char (point-min))
                      (org-reverse-datetree-cleanup-empty-dates :noconfirm t
                                                                :ancestors t)
                      (set-buffer-modified-p nil)
                      (cl-loop for level in (number-sequence 1 4)
                               collect (cons level
                                             (org-reverse-datetree-test--collect-headings level))))))
      (expect (alist-get 4 headings)
              :to-equal '("Bye"
                          "Hello"))
      (expect (alist-get 3 headings)
              :to-equal '("2020-12-10 Thursday"
                          "2020-11-28 Saturday"))
      (expect (alist-get 2 headings)
              :to-equal '("2020-12 December" "2020-11 November"))
      (expect (alist-get 1 headings)
              :to-equal '("Test" "2020")))))

(describe "org-reverse-datetree--entry-time-2"
  (describe "Default"
    (let ((results (with-temp-buffer
                     (insert-file-contents "test/time.org")
                     ;; (setq buffer-file-name "test/time.org")
                     (org-mode)
                     (goto-char (point-min))
                     (let ((org-reverse-datetree-entry-time
                            (eval (car (get 'org-reverse-datetree-entry-time
                                            'standard-value)))))
                       (list (org-reverse-datetree--entry-time-2)
                             (progn
                               (re-search-forward (rx bol "** Child"))
                               (org-reverse-datetree--entry-time-2)))))))
      (it "takes the closed property if available"
        (expect (nth 0 results)
                :to-equal
                (org-reverse-datetree--encode-time
                 (list 0 12 22 26 2 2022 nil nil nil))))
      (it "takes the latest clock finish"
        (expect (nth 1 results)
                :to-equal
                (org-reverse-datetree--encode-time
                 (list 0 5 2 20 1 2022 nil nil nil))))))

  (describe "With an argument"
    (let ((result (with-temp-buffer
                    (insert-file-contents "test/time.org")
                    ;; (setq buffer-file-name "test/time.org")
                    (org-mode)
                    (goto-char (point-min))
                    (org-reverse-datetree--entry-time-2 '((property "CREATED_TIME"))))))
      (it "takes the creation time if available"
        (expect result
                :to-equal
                (org-reverse-datetree--encode-time
                 (list 0 40 15 31 1 2022 nil nil nil)))))
    (let ((result (with-temp-buffer
                    (insert-file-contents "test/time.org")
                    ;; (setq buffer-file-name "test/time.org")
                    (org-mode)
                    (goto-char (point-min))
                    (org-reverse-datetree--entry-time-2 '((clock earliest))))))
      (it "takes the earliest clock if available"
        (expect result
                :to-equal
                (org-reverse-datetree--encode-time
                 (list 0 40 15 31 1 2022 nil nil nil)))))
    (pcase-let ((`(,result-inactive ,result-active)
                 (with-temp-buffer
                   (insert-file-contents "test/time.org")
                   ;; (setq buffer-file-name "test/time.org")
                   (org-mode)
                   (goto-char (org-find-property "CUSTOM_ID" "clock-in-heading-1"))
                   (list (org-reverse-datetree--entry-time-2 '((match :type inactive)))
                         (org-reverse-datetree--entry-time-2 '((match :type active)))))))
      (it "matches the first inactive clock"
        (expect result-inactive
                :to-equal
                (org-reverse-datetree--encode-time
                 (list 0 0 0 10 3 2022 nil nil nil))))
      (it "matches the first active clock"
        (expect result-active
                :to-equal
                (org-reverse-datetree--encode-time
                 (list 0 33 15 24 2 2022 nil nil nil)))))
    (pcase-let ((`(,result-any ,result-default)
                 (with-temp-buffer
                   (insert-file-contents "test/time.org")
                   ;; (setq buffer-file-name "test/time.org")
                   (org-mode)
                   (goto-char (org-find-property "CUSTOM_ID" "clock-in-heading-2"))
                   (list (org-reverse-datetree--entry-time-2 '((match :type any)))
                         (org-reverse-datetree--entry-time-2 '((match :type nil)))))))
      (it "matches the first any clock"
        (expect result-any
                :to-equal
                (org-reverse-datetree--encode-time
                 (list 0 24 12 1 3 2022 nil nil nil))))
      (it "matches the first inactive clock"
        (expect result-default
                :to-equal
                (org-reverse-datetree--encode-time
                 (list 0 0 0 10 3 2022 nil nil nil)))))))

(provide 'org-reverse-datetree-test)
