;;; emt-test.el --- ERT tests for EMT  -*- lexical-binding: t; -*-

(require 'ert)
(require 'subword)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(require 'emt)

(defconst emt-test--module-path
  (expand-file-name (concat "module/target/release/libemt_module" module-file-suffix)
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory (or load-file-name buffer-file-name)))))
  "Path to the locally built EMT test module.")

(defmacro emt-test--with-module (&rest body)
  "Run BODY with the local EMT module loaded."
  `(let ((emt-lib-path emt-test--module-path)
         (emt--lib-loaded nil))
     (unless (file-exists-p emt-lib-path)
       (ert-skip (format "Missing built EMT module at %s" emt-lib-path)))
     (emt-ensure)
     ,@body))

(ert-deftest emt-test-segment-vector ()
  (emt-test--with-module
   (should (equal (append (emt-segment "测试封装功能") nil)
                  '((0 . 2) (2 . 4) (4 . 6))))))

(ert-deftest emt-test-mode-adjusts-forward-word ()
  (emt-test--with-module
   (with-temp-buffer
     (insert "测试封装功能")
     (goto-char (point-min))
     (let ((plain (progn (forward-word 1) (point))))
       (goto-char (point-min))
       (emt-mode 1)
       (forward-word 1)
       (should (= plain 7))
       (should (= (point) 3))))))

(ert-deftest emt-test-transpose-words-with-mode ()
  (emt-test--with-module
   (with-temp-buffer
     (insert "测试封装功能")
     (goto-char (point-min))
     (emt-mode 1)
     (transpose-words 1)
     (should (equal (buffer-string) "封装测试功能")))))

(ert-deftest emt-test-wrapper-motion-works-without-mode ()
  (emt-test--with-module
   (with-temp-buffer
     (insert "测试封装功能")
     (goto-char (point-min))
     (emt-forward-word 1)
     (should (= (point) 3)))))

(ert-deftest emt-test-cache-invalidates-after-edit ()
  (emt-test--with-module
   (with-temp-buffer
     (insert "今天天气")
     (goto-char (point-min))
     (let ((emt-use-cache t))
       (emt-bounds-at-point)
       (let ((tick emt--cache-tick)
             (count (hash-table-count emt--cache)))
         (goto-char (point-max))
         (insert "真好")
         (goto-char (point-min))
         (emt-bounds-at-point)
         (should (= count 1))
         (should (= (hash-table-count emt--cache) 1))
         (should-not (equal tick emt--cache-tick)))))))

(ert-deftest emt-test-restores-existing-word-boundary-table ()
  (emt-test--with-module
   (with-temp-buffer
     (subword-mode 1)
     (let ((original find-word-boundary-function-table))
       (emt-mode 1)
       (emt-mode 0)
       (should (eq original find-word-boundary-function-table))))))

(ert-deftest emt-test-non-han-bounds-return-nil ()
  (emt-test--with-module
   (with-temp-buffer
     (insert "plain ascii")
     (goto-char (point-min))
     (should-not (emt-bounds-at-point)))))

(ert-deftest emt-test-ensure-missing-module-signals-user-error ()
  (let ((emt-lib-path (make-temp-name "emt-missing-module-"))
        (emt--lib-loaded nil))
    (should-error (emt-ensure) :type 'user-error)))

(ert-deftest emt-test-meow-cjk-shims-available ()
  (emt-test--with-module
   (with-temp-buffer
     (insert "测试封装功能")
     (goto-char (point-min))
     (should (eq (emt--move-by-word-decide-bounds-direction 'forward) 'all))
     (should (equal (emt--get-bounds-at-point 'all) '(1 . 7)))
     (forward-char 2)
     (should (equal (emt--get-bounds-at-point 'all) '(1 . 7))))))

(provide 'emt-test)

;;; emt-test.el ends here
