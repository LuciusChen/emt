;;; emt.el --- Tokenizing CJK words with NLP tokenizer  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Roife Wu

;; Author: Roife Wu <roifewu@gmail.com>
;; Maintainer: LuciusChen
;; URL: https://github.com/LuciusChen/emt
;; Version: 3.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: chinese, cjk, tokenizer, natural language, segmentation

;; This file is NOT a part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A fork of EMT (https://github.com/roife/emt) with cross-platform support
;; via a built-in Rust dynamic module using jieba-rs for Chinese word
;; segmentation.
;;
;; Supports macOS, Linux, and Windows.

;;; Code:

(eval-when-compile (require 'thingatpt))
(eval-when-compile (require 'subr-x))

;;; Customize

(defgroup emt ()
  "Tokenize CJK words with NLP tokenizer."
  :group 'chinese
  :prefix "emt-")

(defcustom emt-use-cache t
  "Caches for results of tokenization if non-nil."
  :type 'boolean
  :group 'emt)

(defcustom emt-cache-lru-size 50
  "The size of LRU cache for tokenization results."
  :type 'integer
  :group 'emt)

(defcustom emt-lib-path
  (concat user-emacs-directory "modules/libEMT" module-file-suffix)
  "The path to the dynamic library for emt."
  :type 'string
  :group 'emt)

;;; Internal variables

(defconst emt--module-version "v0.1.0"
  "The version of the emt dynamic module.")

(defvar emt--root (file-name-directory (or load-file-name buffer-file-name))
  "The path to the root of the package.")

(defvar emt--cjk-regex-forward "\\(\\cc\\|\\cj\\|\\ch\\)+\\W*$"
  "Forward regex for CJK.")

(defvar emt--cjk-regex-backward "^\\W*\\(\\cc\\|\\cj\\|\\ch\\)+"
  "Backward regex for CJK.")

(defvar emt--lib-loaded nil
  "Whether dynamic module for emt is loaded.")

(defvar emt--cache-set (make-hash-table :test #'equal)
  "The hash table for caching tokenization results.")

(defvar emt--cache-lru-list nil
  "The LRU list for caching tokenization results.")

(defvar emt--lib-fns
  '(emt--do-split-helper
    emt--word-at-point-or-forward-helper)
  "The list of functions that must exist in the dynamic module.")

;;; Cache

(defun emt--cache-get (key)
  "Get the value of KEY in cache."
  (let ((leading 0))
    (when (zerop (string-match "\\W*" key))
      (setq leading (match-end 0)))
    (setq key (string-trim (substring-no-properties key) "\\W*" "\\W*"))
    (let ((value (gethash key emt--cache-set)))
      (when value
        (setq emt--cache-lru-list (delete key emt--cache-lru-list))
        (push key emt--cache-lru-list))
      (if (zerop leading)
          value
        (mapcar (lambda (pair) (cons (+ (car pair) leading)
                                     (+ (cdr pair) leading)))
                value)))))

(defun emt--cache-put (key value)
  "Put KEY and VALUE into cache."
  (let ((leading 0))
    (when (zerop (string-match "\\W*" key))
      (setq leading (match-end 0)))
    (unless (zerop leading)
      (setq value (mapcar (lambda (pair) (cons (- (car pair) leading)
                                               (- (cdr pair) leading)))
                          value)))
    (setq key (string-trim (substring-no-properties key) "\\W*" "\\W*"))
    (puthash key value emt--cache-set)
    (push key emt--cache-lru-list)
    (when (> (length emt--cache-lru-list) emt-cache-lru-size)
      (setq emt--cache-lru-list (butlast emt--cache-lru-list)))))

;;; Helpers

(defun emt--get-bounds-at-point (direction)
  "Get the bounds of the CJK string at point.

DIRECTION can be `forward', `backward', or `all'."
  (let* ((pos (point))
         (beg pos)
         (end pos))
    (when (or (eq direction 'forward) (eq direction 'all))
      (save-excursion
        (forward-word)
        (when (string-match-p emt--cjk-regex-forward
                              (buffer-substring-no-properties pos (point)))
          (setq end (point)))))
    (when (or (eq direction 'backward) (eq direction 'all))
      (save-excursion
        (backward-word)
        (when (string-match-p emt--cjk-regex-backward
                              (buffer-substring-no-properties (point) pos))
          (setq beg (point)))))
    (cons beg end)))

(defun emt--word-at-point (back)
  "Return the word at point.  If BACK is non-nil, return the word backward."
  (unless emt--lib-loaded (error "Dynamic module not loaded"))
  (pcase-let* ((`(,beg . ,end) (emt--get-bounds-at-point 'all))
               (index (- (point) beg))
               (`(,word-beg . ,word-end)
                (emt--word-at-point-or-forward-helper
                 (buffer-substring-no-properties beg end)
                 (if (and back (> index 0)) (1- index) index))))
    (if (eq word-beg word-end)
        (word-at-point)
      (buffer-substring (+ beg word-beg) (+ beg word-end)))))

(defun emt--upperbound (pred vec)
  "Binary search for the last element in VEC satisfying PRED."
  (if (zerop (length vec)) nil
    (let ((start 0)
          (end (1- (length vec))))
      (while (< start end)
        (let ((mid (ash (+ start end 1) -1)))
          (if (funcall pred (elt vec mid))
              (setq start mid)
            (setq end (1- mid)))))
      (elt vec start))))

(defun emt--lowerbound (pred vec)
  "Binary search for the first element in VEC satisfying PRED."
  (if (zerop (length vec)) nil
    (let ((start 0)
          (end (1- (length vec))))
      (while (< start end)
        (let ((mid (ash (+ start end) -1)))
          (if (funcall pred (elt vec mid))
              (setq end mid)
            (setq start (1+ mid)))))
      (elt vec end))))

(defun emt--move-by-word-decide-bounds-direction (direction)
  "Decide the CJK bounds direction based on movement DIRECTION."
  (if (eq direction 'forward)
      (if (and (char-after) (string-match "\\W" (char-to-string (char-after))))
          'forward
        'all)
    (if (and (char-before) (string-match "\\W" (char-to-string (char-before))))
        'backward
      'all)))

(defun emt--move-by-word (direction)
  "Move point by one CJK word in DIRECTION (`forward' or `backward')."
  (pcase-let ((forward-p (eq direction 'forward))
              (`(,beg . ,end)
               (emt--get-bounds-at-point
                (emt--move-by-word-decide-bounds-direction direction))))
    (if (eq beg end)
        (if forward-p (forward-word) (backward-word))
      (let* ((text (buffer-substring-no-properties beg end))
             (pos (- (point) beg))
             (pred (if forward-p (lambda (x) (> x pos)) (lambda (x) (< x pos))))
             (target-bound
              (funcall (if forward-p #'emt--lowerbound #'emt--upperbound)
                       pred
                       (mapcar (if forward-p #'cdr #'car) (emt-split text)))))
        (if (and target-bound (funcall pred target-bound))
            (goto-char (+ beg target-bound))
          (if forward-p (forward-word) (backward-word)))))))

;;; Public API

(defun emt-split (str)
  "Split STR into a list of word bounds (BEG . END)."
  (if emt--lib-loaded
      (if-let ((cached (and emt-use-cache (emt--cache-get str))))
          cached
        (let ((result (emt--do-split-helper str)))
          (when emt-use-cache (emt--cache-put str result))
          result))
    (error "Dynamic module not loaded")))

(defun emt-get-arch ()
  "Return the CPU architecture string for the current system."
  (cond
   ((string-match-p "aarch64\\|arm64" system-configuration) "aarch64")
   ((string-match-p "x86_64"          system-configuration) "x86_64")
   (t (error "Unsupported architecture: %s" system-configuration))))

;;;###autoload
(defun emt-word-at-point-or-forward ()
  "Return the CJK word at point, or the one forward if at a boundary."
  (interactive)
  (emt--word-at-point nil))

;;;###autoload
(defun emt-word-at-point-or-backward ()
  "Return the CJK word at point, or the one backward if at a boundary."
  (interactive)
  (emt--word-at-point t))

;;;###autoload
(defun emt-download-module (&optional path)
  "Download the pre-built emt dynamic module from GitHub releases.

The module is placed at PATH, defaulting to `emt-lib-path'.
Supports macOS (aarch64/x86_64), Linux (x86_64), and Windows (x86_64)."
  (interactive)
  (setq path (or path emt-lib-path))
  (make-directory (file-name-directory path) t)
  (let* ((arch (emt-get-arch))
         (triple
          (cond
           ((eq system-type 'darwin)
            (concat arch "-apple-darwin"))
           ((eq system-type 'gnu/linux)
            (concat arch "-unknown-linux-gnu"))
           ((eq system-type 'windows-nt)
            (concat arch "-pc-windows-msvc"))
           (t (error "Unsupported platform: %s" system-type))))
         (lib-filename (concat "libEMT-" triple
                               (if (eq system-type 'windows-nt) ".dll"
                                 (if (eq system-type 'darwin) ".dylib" ".so"))))
         (url (format "https://github.com/LuciusChen/emt/releases/download/%s/%s"
                      emt--module-version lib-filename)))
    (message "Downloading emt module from %s ..." url)
    (url-copy-file url path t)
    (message "EMT module installed to %s" path)))

;;;###autoload
(defun emt-forward-word (&optional arg)
  "CJK-compatible `forward-word', moving ARG words."
  (interactive "^p")
  (setq arg (or arg 1))
  (let ((direction (if (< arg 0) 'backward 'forward))
        (count (abs arg))
        (i 0))
    (while (and (< i count) (emt--move-by-word direction))
      (setq i (1+ i)))
    (eq i count)))

;;;###autoload
(defun emt-backward-word (&optional arg)
  "CJK-compatible `backward-word', moving ARG words."
  (interactive "^p")
  (setq arg (or arg 1))
  (emt-forward-word (- arg)))

;;;###autoload
(defun emt-kill-word (arg)
  "CJK-compatible `kill-word', killing ARG words forward."
  (interactive "p")
  (kill-region (point) (progn (emt-forward-word arg) (point))))

;;;###autoload
(defun emt-backward-kill-word (arg)
  "CJK-compatible `backward-kill-word', killing ARG words backward."
  (interactive "p")
  (emt-kill-word (- arg)))

;;;###autoload
(defun emt-mark-word (&optional arg)
  "CJK-compatible `mark-word', marking ARG words."
  (interactive "p")
  (set-mark (point))
  (emt-forward-word arg))

;;;###autoload
(defun emt-ensure ()
  "Ensure the dynamic module is loaded, downloading if necessary."
  (interactive)
  (unless emt--lib-loaded
    (unless (file-exists-p emt-lib-path)
      (if (yes-or-no-p "EMT module not found. Download pre-built from GitHub? ")
          (emt-download-module)
        (error "EMT module cannot be loaded")))
    (load-file emt-lib-path)
    (dolist (fn emt--lib-fns)
      (unless (fboundp fn)
        (error "No %s function found in dynamic module" fn)))
    (setq emt--lib-loaded t)))

;;; Minor mode

;;;###autoload
(defvar emt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap forward-word]          #'emt-forward-word)
    (define-key map [remap backward-word]         #'emt-backward-word)
    (define-key map [remap kill-word]             #'emt-kill-word)
    (define-key map [remap backward-kill-word]    #'emt-backward-kill-word)
    (define-key map [remap mark-word]             #'emt-mark-word)
    (define-key map [remap word-at-point]         #'emt-word-at-point-or-forward)
    map))

;;;###autoload
(define-minor-mode emt-mode
  "Minor mode for CJK word tokenization via jieba-rs."
  :global t
  :keymap emt-mode-map
  :lighter "emt"
  (when emt-mode (emt-ensure)))

(provide 'emt)

;;; emt.el ends here
