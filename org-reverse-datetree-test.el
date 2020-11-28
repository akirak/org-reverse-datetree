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

(provide 'org-reverse-datetree-test)
