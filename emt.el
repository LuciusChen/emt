;;; emt.el --- Han word boundaries via jieba-rs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Roife Wu

;; Author: Roife Wu <roifewu@gmail.com>
;; Maintainer: LuciusChen
;; URL: https://github.com/LuciusChen/emt
;; Version: 0.1.0
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

;; EMT provides Han word boundaries backed by jieba-rs.  It keeps the `emt'
;; package name and compatibility surface for existing users.  In `emt-mode',
;; it integrates with `find-word-boundary-function-table' so built-in word
;; motion commands like `forward-word' and `kill-word' become segmentation
;; aware for Chinese text.  Compatibility wrappers are retained for callers
;; that want EMT behavior without enabling the minor mode.

;;; Code:

(require 'thingatpt)

(declare-function emt--segment nil (str))

;;; Customize

(defgroup emt ()
  "Han word boundaries backed by jieba-rs."
  :group 'chinese
  :prefix "emt-")

(defcustom emt-use-cache t
  "Cache segmentation results for contiguous Han runs if non-nil."
  :type 'boolean
  :group 'emt)

(defcustom emt-cache-lru-size 50
  "Obsolete compatibility option for the pre-rewrite global cache.

This value is ignored.  EMT now uses a per-buffer cache that is invalidated
by `buffer-chars-modified-tick'."
  :type 'integer
  :group 'emt)

(defcustom emt-lib-path
  (expand-file-name (concat "modules/libemt_module" module-file-suffix)
                    user-emacs-directory)
  "The path to the dynamic library for emt."
  :type 'string
  :group 'emt)

;;; Internal variables

(defconst emt--module-version "v0.1.0"
  "The version of the emt dynamic module.")

(defconst emt--han-ranges
  '((#x3400 . #x4DBF)
    (#x4E00 . #x9FFF)
    (#xF900 . #xFAFF)
    (#x20000 . #x2A6DF)
    (#x2A700 . #x2B73F)
    (#x2B740 . #x2B81F)
    (#x2B820 . #x2CEAF)
    (#x2CEB0 . #x2EBEF)
    (#x2EBF0 . #x2EE5F)
    (#x2F800 . #x2FA1F)
    (#x30000 . #x3134F)
    (#x31350 . #x323AF))
  "Unicode ranges treated as Han by EMT.")

(defconst emt--empty-char-table
  (make-char-table nil)
  "Used to disable EMT's boundary hook while EMT is already searching.")

(defconst emt--word-boundary-function-table
  (let ((tab (make-char-table nil)))
    (dolist (range emt--han-ranges)
      (set-char-table-range tab range #'emt--find-word-boundary))
    tab)
  "Boundary handlers installed by `emt-mode' for Han characters.")

(defvar emt--lib-loaded nil
  "Whether the dynamic module for EMT is loaded.")

(defvar emt--lib-fns
  '(emt--segment
    emt--do-split-helper)
  "Functions that must exist in the dynamic module.")

(defvar-local emt--cache nil
  "Buffer-local cache for Han run segmentation results.")

(defvar-local emt--cache-tick nil
  "Modification tick associated with `emt--cache'.")

(defvar-local emt--saved-find-word-boundary-function-table nil
  "Original buffer-local word boundary table saved by `emt-mode'.")

(defvar-local emt--saved-find-word-boundary-function-table-was-local nil
  "Whether `find-word-boundary-function-table' was buffer-local before EMT.")

;;; Helpers

(defmacro emt--with-word-boundaries (&rest body)
  "Evaluate BODY with EMT's boundary table enabled for the current buffer."
  `(let ((find-word-boundary-function-table
          (emt--compose-word-boundary-function-table
           find-word-boundary-function-table)))
     ,@body))

(defun emt--word-boundary-supported-p ()
  "Return non-nil when this Emacs has word-boundary hook support."
  (boundp 'find-word-boundary-function-table))

(defun emt--ensure-word-boundary-support ()
  "Signal an error if this Emacs cannot install EMT's boundary handlers."
  (unless (emt--word-boundary-supported-p)
    (error "EMT requires Emacs with find-word-boundary-function-table support")))

(defun emt--han-char-p (char)
  "Return non-nil if CHAR is handled by EMT."
  (and char (functionp (aref emt--word-boundary-function-table char))))

(defun emt--compose-word-boundary-function-table (base-table)
  "Return a boundary table that preserves BASE-TABLE and adds EMT handlers."
  (let ((table (if (char-table-p base-table)
                   (copy-sequence base-table)
                 (make-char-table nil))))
    (dolist (range emt--han-ranges table)
      (set-char-table-range table range #'emt--find-word-boundary))))

(defun emt--ensure-cache ()
  "Return the current buffer cache, invalidating it after edits."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (hash-table-p emt--cache)
      (setq emt--cache (make-hash-table :test #'equal)
            emt--cache-tick tick))
    (unless (equal emt--cache-tick tick)
      (clrhash emt--cache)
      (setq emt--cache-tick tick))
    emt--cache))

(defun emt--cjk-run-at (pos)
  "Return the contiguous Han run containing POS as (BEG . END), or nil."
  (save-excursion
    (goto-char pos)
    (when (emt--han-char-p (char-after))
      (let ((end (progn
                   (while (emt--han-char-p (char-after))
                     (forward-char 1))
                   (point))))
        (while (emt--han-char-p (char-before))
          (backward-char 1))
        (cons (point) end)))))

(defun emt--segment-index-at (segments offset)
  "Return the index in SEGMENTS whose half-open range contains OFFSET."
  (let ((low 0)
        (high (1- (length segments)))
        found)
    (while (and (not found) (<= low high))
      (let* ((mid (+ low (ash (- high low) -1)))
             (segment (aref segments mid))
             (beg (car segment))
             (end (cdr segment)))
        (cond
         ((< offset beg)
          (setq high (1- mid)))
         ((>= offset end)
          (setq low (1+ mid)))
         (t
          (setq found mid)))))
    found))

(defun emt--segment-run (run-beg run-end)
  "Return a segment vector for the Han run between RUN-BEG and RUN-END."
  (let ((text (buffer-substring-no-properties run-beg run-end)))
    (if emt-use-cache
        (let* ((cache (emt--ensure-cache))
               (key (cons run-beg run-end))
               (cached (gethash key cache)))
          (or cached
              (let ((segments (emt--segment text)))
                (puthash key segments cache)
                segments)))
      (emt--segment text))))

(defun emt--bounds-at-buffer-position (pos)
  "Return the Han word bounds containing POS, or nil."
  (when-let* ((run (emt--cjk-run-at pos))
              (segments (emt--segment-run (car run) (cdr run)))
              (index (emt--segment-index-at segments (- pos (car run)))))
    (let* ((segment (aref segments index))
           (run-beg (car run)))
      (cons (+ run-beg (car segment))
            (+ run-beg (cdr segment))))))

(defun emt--bounds-at-point-internal (direction)
  "Return Han word bounds around point.

If DIRECTION is `forward', prefer the word after point when point is on a
boundary.  If DIRECTION is `backward', prefer the word before point."
  (let ((pos (point)))
    (or (and (eq direction 'backward)
             (> pos (point-min))
             (emt--bounds-at-buffer-position (1- pos)))
        (and (< pos (point-max))
             (emt--bounds-at-buffer-position pos))
        (and (> pos (point-min))
             (emt--bounds-at-buffer-position (1- pos))))))

(defun emt--move-by-word-decide-bounds-direction (direction)
  "Compatibility helper kept for downstream callers.

Return a legacy bounds lookup DIRECTION for word movement."
  (if (eq direction 'forward)
      (if (emt--han-char-p (char-after))
          'all
        'forward)
    (if (or (emt--han-char-p (char-after))
            (emt--han-char-p (char-before)))
        'all
      'backward)))

(defun emt--get-bounds-at-point (direction)
  "Compatibility helper returning Han run bounds around point.

DIRECTION may be `forward', `backward', or `all'.  The return value is a
cons cell (BEG . END).  If no Han run is available for the requested
direction, return (POINT . POINT)."
  (let* ((pos (point))
         (run
          (pcase direction
            ('forward
             (and (emt--han-char-p (char-after))
                  (emt--cjk-run-at pos)))
            ('backward
             (or (and (emt--han-char-p (char-after))
                      (emt--cjk-run-at pos))
                 (and (> pos (point-min))
                      (emt--han-char-p (char-before))
                      (emt--cjk-run-at (1- pos)))))
            (_
             (or (and (emt--han-char-p (char-after))
                      (emt--cjk-run-at pos))
                 (and (> pos (point-min))
                      (emt--han-char-p (char-before))
                      (emt--cjk-run-at (1- pos))))))))
    (or run (cons pos pos))))

(defun emt--word-at-point (direction)
  "Return the word at point, preferring DIRECTION when on a boundary."
  (if-let* ((bounds (emt--bounds-at-point-internal direction)))
      (buffer-substring-no-properties (car bounds) (cdr bounds))
    (word-at-point t)))

(defun emt--enable-mode ()
  "Enable `emt-mode' in the current buffer."
  (emt--ensure-word-boundary-support)
  (emt-ensure)
  (setq emt--saved-find-word-boundary-function-table
        find-word-boundary-function-table
        emt--saved-find-word-boundary-function-table-was-local
        (local-variable-p 'find-word-boundary-function-table (current-buffer)))
  (setq-local find-word-boundary-function-table
              (emt--compose-word-boundary-function-table
               find-word-boundary-function-table)))

(defun emt--disable-mode ()
  "Disable `emt-mode' in the current buffer."
  (if emt--saved-find-word-boundary-function-table-was-local
      (setq-local find-word-boundary-function-table
                  emt--saved-find-word-boundary-function-table)
    (kill-local-variable 'find-word-boundary-function-table))
  (setq emt--saved-find-word-boundary-function-table nil
        emt--saved-find-word-boundary-function-table-was-local nil))

(defun emt--find-word-boundary (pos limit)
  "Boundary handler installed in `find-word-boundary-function-table'.

POS and LIMIT follow the contract of `find-word-boundary-function-table'."
  (let ((find-word-boundary-function-table emt--empty-char-table))
    (if-let* ((run (emt--cjk-run-at pos))
              (segments (emt--segment-run (car run) (cdr run)))
              (index (emt--segment-index-at segments (- pos (car run)))))
        (let* ((segment (aref segments index))
               (run-beg (car run))
               (beg (+ run-beg (car segment)))
               (end (+ run-beg (cdr segment))))
          (if (< pos limit)
              (min end limit)
            (max beg limit)))
      (if (< pos limit) limit pos))))

;;; Public API

(defun emt-segment (str)
  "Split STR into a vector of word bounds (BEG . END)."
  (unless emt--lib-loaded
    (error "Dynamic module not loaded"))
  (let ((segments (emt--segment str)))
    (if (vectorp segments)
        segments
      (vconcat segments))))

(defun emt-split (str)
  "Split STR into a list of word bounds (BEG . END)."
  (append (emt-segment str) nil))

(defun emt-get-arch ()
  "Return the CPU architecture string for the current system."
  (cond
   ((string-match-p "aarch64\\|arm64" system-configuration) "aarch64")
   ((string-match-p "x86_64" system-configuration) "x86_64")
   (t (error "Unsupported architecture: %s" system-configuration))))

;;;###autoload
(defun emt-bounds-at-point ()
  "Return the Han word bounds at point as (BEG . END), or nil."
  (interactive)
  (emt-ensure)
  (emt--bounds-at-point-internal 'forward))

;;;###autoload
(defun emt-word-at-point-or-forward ()
  "Return the Han word at point, or the one forward if at a boundary."
  (interactive)
  (emt-ensure)
  (emt--word-at-point 'forward))

;;;###autoload
(defun emt-word-at-point-or-backward ()
  "Return the Han word at point, or the one backward if at a boundary."
  (interactive)
  (emt-ensure)
  (emt--word-at-point 'backward))

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
  "Move forward by ARG words using EMT's Han boundary rules."
  (interactive "^p")
  (emt--ensure-word-boundary-support)
  (emt-ensure)
  (emt--with-word-boundaries
   (forward-word (or arg 1))))

;;;###autoload
(defun emt-backward-word (&optional arg)
  "Move backward by ARG words using EMT's Han boundary rules."
  (interactive "^p")
  (emt--ensure-word-boundary-support)
  (emt-ensure)
  (emt--with-word-boundaries
   (backward-word (or arg 1))))

;;;###autoload
(defun emt-kill-word (arg)
  "Kill ARG words forward using EMT's Han boundary rules."
  (interactive "p")
  (kill-region (point) (progn (emt-forward-word arg) (point))))

;;;###autoload
(defun emt-backward-kill-word (arg)
  "Kill ARG words backward using EMT's Han boundary rules."
  (interactive "p")
  (emt-kill-word (- arg)))

;;;###autoload
(defun emt-mark-word (&optional arg)
  "Mark ARG words using EMT's Han boundary rules."
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
(define-minor-mode emt-mode
  "Buffer-local minor mode for Han word boundaries via jieba-rs."
  :lighter " emt"
  (if emt-mode
      (emt--enable-mode)
    (emt--disable-mode)))

;;;###autoload
(define-globalized-minor-mode global-emt-mode emt-mode
  (lambda ()
    (unless (minibufferp)
      (emt-mode 1))))

(provide 'emt)

;;; emt.el ends here
