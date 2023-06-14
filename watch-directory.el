;;; watch-directory.el --- watch directories and insert stuff  -*- lexical-binding: t -*-
;; Copyright (C) 2015 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions, processes

;; This file is not part of GNU Emacs.

;; watch-directory.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; watch-directory.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'meme)

(defvar watch-directory-rescale nil)

(defvar watch-directory-trim t)

(defvar watch-directory-trim-fuzz "4%")

(defun watch-directory--image-type ()
  (if (or (and (fboundp 'image-transforms-p)
	       (image-transforms-p))
	  (not (fboundp 'imagemagick-types)))
      nil
    'imagemagick))

(defun watch-directory (directory &optional match no-ignore-existing
				  separator)
  "Watch DIRECTORY for new files and insert them in the buffer when they appear.
If MATCH, insert the files that match this name.  Defaults to .JPG."
  (interactive "DDirectory to watch: ")
  (let ((files (if no-ignore-existing
		   nil
		 (directory-files directory t)))
	(buffer (current-buffer))
	timer new)
    (setq timer
	  (run-at-time
	   1 1
	   (lambda ()
	     (if (not (buffer-live-p buffer))
		 (cancel-timer timer)
	       (dolist (file (directory-files directory t))
		 (when (and (not (member file files))
			    (string-match (or match ".JPG$")
					  (file-name-nondirectory file))
			    (plusp (file-attribute-size
				    (file-attributes file)))
			    (watch-directory--file-complete-p file))
		   (when (or watch-directory-rescale
			     watch-directory-trim)
		     (let ((crop (watch-directory--find-crop
				  buffer directory (or match "[.]jpg$")
				  files)))
		       (setq new (watch-directory-uniqify
				  (expand-file-name
				   (file-name-nondirectory file) "/tmp")))
		       (apply
			#'call-process
			`("convert" nil nil nil
			  ,@(if watch-directory-rescale '("-scale" "2048x"))
			  ,@(if watch-directory-trim
				(if crop
				    ;; If we have a crop factor, use it.
				    (list "-crop"
					  (apply #'format "%dx%d+%d+%d" crop))
				  ;; If not, auto-fuzz.
				  (list "-trim" "-fuzz"
					watch-directory-trim-fuzz)))
			  ,file ,new)))
		     (push file files)
		     (setq file new))
		   (with-current-buffer buffer
		     (let ((edges (window-inside-pixel-edges
				   (get-buffer-window (current-buffer) t))))
		       (save-excursion
			 (goto-char (point-max))
			 ;; If we're just after an image, leave
			 ;; several empty lines (to type in).  If not,
			 ;; just one empty line.
			 (if (save-excursion
			       (and (re-search-backward "^[^\n]" nil t)
				    (looking-at "<img")))
			     (ensure-empty-lines 3)
			   (ensure-empty-lines 1))
			 (insert-image
			  (create-image
			   file (watch-directory--image-type) nil
			   :max-width
			   (truncate
			    (* 0.95 (- (nth 2 edges) (nth 0 edges))))
			   :max-height
			   (truncate
			    (* 0.7 (- (nth 3 edges) (nth 1 edges)))))
			  (format "<img src=%S>" file))
			 (insert "\n\n")
			 (when separator
			   (insert separator)))))
		   ;; Keep track of the inserted files.
		   (push file files)))))))
    timer))

(defun watch-directory--file-complete-p (file)
;;  (copy-file file "/tmp/lala.jpg" t)
  (with-temp-buffer
    (zerop (call-process "identify" nil t nil
			 "-regard-warnings" file))))

(defvar-local watch-directory--crop-factor nil)

(defun watch-directory--find-crop (buffer dir match ignore-files)
  (with-current-buffer buffer
    (or watch-directory--crop-factor
	(and
	 watch-directory-trim
	 (let* ((files (seq-take (seq-difference (directory-files dir t match)
						 ignore-files)
				 10))
		(crop (meme--find-crop files)))
	   ;; If we have a reasonable number of files, then cache the
	   ;; results.
	   (when (> (length files) 9)
	     (setq-local watch-directory--crop-factor crop))
	   crop)))))

(defun watch-directory-uniqify (file)
  (let ((num 2))
    (while (file-exists-p file)
      (setq file (replace-regexp-in-string "\\(-[0-9]+\\)?[.]"
					   (format "-%d." num) file))
      (cl-incf num)))
  file)

(provide 'watch-directory)

;;; watch-directory.el ends here
