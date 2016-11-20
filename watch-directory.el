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

(defun watch-directory (directory)
  "Watch DIRECTORY for new files and insert them in the buffer when they appear."
  (interactive "DDirectory to watch: ")
  (let ((files (directory-files directory t))
	(buffer (current-buffer))
	timer)
    (setq timer
	  (run-at-time
	   1 1
	   (lambda ()
	     (if (not (buffer-live-p buffer))
		 (cancel-timer timer)
	       (dolist (file (directory-files directory t))
		 (when (and (not (member file files))
			    (string-match ".JPG$" file)
			    (plusp (file-attribute-size
				    (file-attributes file))))
		   (with-current-buffer buffer
		     (save-excursion
		       (goto-char (point-max))
		       (let ((point (point))
			     (edges (window-inside-pixel-edges
				     (get-buffer-window (current-buffer)))))
			 (insert (format "<img src=%S>\n\n" file))
			 (put-image
			  (create-image
			   file 'imagemagick nil
			   :max-width
			   (truncate
			    (* 0.7 (- (nth 2 edges) (nth 0 edges))))
			   :max-height
			   (truncate
			    (* 0.5 (- (nth 3 edges) (nth 1 edges)))))
			  point))))
		   (push file files)))))))
    timer))

(provide 'watch-directory)

;;; watch-directory.el ends here
