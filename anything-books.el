;;; anything-books.el --- Anything command for PDF books

;; Copyright (C) 2010  SAKURAI Masashi

;; Author: SAKURAI Masashi <sakurai at kiwanami.net>
;; Keywords: anything, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

(require 'cl)
(require 'concurrent)

;;; Code:

(defvar anything-books-books-dir nil)
(defvar anything-books-cache-dir ".cache")
(defvar anything-books-cache-pixel "600")
(defvar anything-books-open-command "acroread")
(defvar anything-books-preview-temp-dir "/tmp" "A directory to save a preview image temporally.")

(defvar anything-books-cmd-copy "cp" "Copy command")
(defvar anything-books-copy-by-command t "If non-nil, this program copies files by the external command asynchronously. If nil, this program uses Emacs copy function `copy-file' synchronously.")

;;; for debug

(eval-and-compile
  (defvar anything-books-debug nil "Debug output switch.")) ; debug
(defvar anything-books-debug-count 0 "[internal] Debug output counter.") ; debug

(defmacro anything-books-log (&rest args)
  (when anything-books-debug
    `(progn 
       (with-current-buffer (get-buffer-create "*anything-books-debug*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "%5i %s\n" anything-books-debug-count (format ,@args)))))
       (incf anything-books-debug-count))))

(defun anything-books-message-mark ()
  (interactive)
  (anything-books-log "==================== mark ==== %s" 
                      (format-time-string "%H:%M:%S" (current-time))))

(defun anything-books-debug-report-semaphore ()
  (interactive)
  (message
   "Semaphore: preview permit: %s / waiting: %s"
   (cc:semaphore-permits anything-books-preview-semaphore)
   (length (cc:semaphore-waiting-deferreds anything-books-preview-semaphore))))

;;; Utility

(defun anything-books-file-exists-p (file)
  (and (file-exists-p file)
       (< 0 (nth 7 (file-attributes file)))))

(defun anything-books-fix-directory ()
  "Make a directory for cache files in the current directory which has visiting file."
  (let ((img-dir (expand-file-name anything-books-cache-dir anything-books-books-dir)))
    (unless (file-directory-p img-dir)
      (make-directory img-dir))
    (unless (file-directory-p img-dir)
      (error "Could not create a image directory."))
    img-dir))

(defun anything-books-get-cache-path (path)
  (let ((file-head (file-name-sans-extension 
                    (file-name-nondirectory path))))
    (expand-file-name 
     (concat file-head ".jpg")
     (anything-books-fix-directory))))

(defun anything-books-copy-file-d (d from-path to-path)
  (unless d (setq d (deferred:next 'identity)))
  (anything-books-log ">>   local copy : %s -> %s" from-path to-path)
  (lexical-let ((from-path from-path) (to-path to-path))
    (deferred:$
      (if anything-books-copy-by-command
          (deferred:processc d anything-books-cmd-copy from-path to-path)
        (deferred:nextc d
          (lambda (x) (ignore-errors (copy-file from-path to-path t t)))))
      (deferred:nextc it
        (lambda (x) 
          (unless (anything-books-file-exists-p to-path)
            (error "Can not copy the file : %s -> %s" from-path to-path))
          to-path)))))

;;; anything preview

;;; Cache Control

(defvar anything-books-preview-image-cache nil "[internal]")
(defvar anything-books-preview-image-cache-num 10 "[internal]")

(defun anything-books-preview-image-cache-get-mru (path)
  (let ((cached-pair (assoc path anything-books-preview-image-cache)))
    (when cached-pair
      (setq anything-books-preview-image-cache
            (cons cached-pair
                  (loop for i in anything-books-preview-image-cache
                        for ipath = (car i)
                        with count = 1
                        unless (or (equal path ipath) 
                                   (<= anything-books-preview-image-cache-num count))
                        collect (progn (incf count) i)))))
    (cdr cached-pair)))

(defun anything-books-preview-image-cache-add (path image)
  (push (cons path image) anything-books-preview-image-cache)
  image)

;;; Preview Buffer

(defvar anything-books-anything-channel nil "[internal]")
(defconst anything-books-preview-buffer " *anything-books-preview*")

(defun anything-books-preview-buffer-init (title)
  (let ((buf (get-buffer anything-books-preview-buffer)))
    (unless buf
      (setq buf (get-buffer-create anything-books-preview-buffer))
      (with-current-buffer buf
        (buffer-disable-undo buf)
        (set (make-local-variable 'preview-title) "")
        (set (make-local-variable 'preview-progress) "")
        (set (make-local-variable 'preview-count) 0))
      (unless anything-books-anything-channel
        (setq anything-books-anything-channel (cc:signal-channel 'anything-books-anything)))
      (cc:signal-disconnect-all anything-books-anything-channel)
      (loop for i in '((show-image . anything-books-preview-buffer-on-show-image)
                       (progress . anything-books-preview-buffer-on-show-progress)
                       (animation . anything-books-preview-buffer-on-show-animation)
                       (image-load-start . anything-books-preview-buffer-start-animation)
                       (image-load-finish . anything-books-preview-buffer-stop-animation))
            for ev = (car i)
            for f = (cdr i)
            do (cc:signal-connect anything-books-anything-channel ev f)))
    (cc:signal-send anything-books-anything-channel 'show-image title nil nil)
    buf))

(defun anything-books-preview-buffer-on-show-image (args)
  (with-current-buffer (get-buffer anything-books-preview-buffer)
    (destructuring-bind (event (title path img)) args
      (anything-books-log "%s %s" title path)
      (setq preview-title title
            preview-count 0
            preview-progress "")
      (anything-books-preview-buffer-update-mode-line)
      (erase-buffer)
      (cond 
       (path
        (insert " ")
        (add-text-properties (point-min) (point-max) (list 'display img))
        (goto-char (point-min)))
       (t
        (insert "No image..."))))))

(defconst anything-books-preview-mode-line-format "%s %5s %s") ; animation, progress, title

(defun anything-books-preview-buffer-update-mode-line ()
  (let ((anm "-/|\\"))
    (setq mode-line-format 
          (format anything-books-preview-mode-line-format
                  (char-to-string 
                   (aref anm (% preview-count (length anm))))
                  preview-progress preview-title)))
    (force-mode-line-update))

(defun anything-books-preview-buffer-on-show-progress (args)
  (with-current-buffer (get-buffer anything-books-preview-buffer)
    (destructuring-bind (event (progress)) args
      (setq preview-progress progress)
      (anything-books-preview-buffer-update-mode-line))))

(defun anything-books-preview-buffer-on-show-animation (buf)
  (with-current-buffer (get-buffer anything-books-preview-buffer)
    (incf preview-count)
    (anything-books-preview-buffer-update-mode-line)))


(defvar anything-books-preview-buffer-thread nil "[internal]")

(defun anything-books-preview-buffer-stop-animation ()
  (setq anything-books-preview-buffer-thread nil))

(defun anything-books-preview-buffer-start-animation ()
  (unless anything-books-preview-buffer-thread
    (setq anything-books-preview-buffer-thread t)
    (cc:thread 
     60 
     (while anything-books-preview-buffer-thread
       (cc:signal-send anything-books-anything-channel 'animation)))))


(defun anything-books-preview-progress (d current total)
  (lexical-let
      ((progress (apply 'concat
             (loop for i from 1 to total
                   collect (if (<= i current) "O" ".")))))
    (deferred:nextc (or d (deferred:succeed))
      (lambda (x)
        (cc:signal-send anything-books-anything-channel 'progress progress)
        x))))

(defun anything-books-preview-image-create-d (d path)
  "Translate a PDF file to JPEG file."
  (anything-books-log ">> anything-books-preview-image-create-d : %s" path)
  (lexical-let
      ((path path) 
       (org-file (expand-file-name "_preview_org-000.jpg" anything-books-preview-temp-dir))
       (org-head (expand-file-name "_preview_org" anything-books-preview-temp-dir))
       (cache-file (anything-books-get-cache-path path)))
    (cond
     ((anything-books-file-exists-p cache-file)
      (anything-books-log ">>   cache file... : %s" cache-file)
      (deferred:nextc d
        (lambda (x) cache-file)))
     (t
      (anything-books-log ">>   converting... : %s" path)
      (deferred:$ d
        (anything-books-preview-progress it 1 4)
        (deferred:nextc it
          (lambda (x)
            (cc:signal-send anything-books-anything-channel 'image-load-start)
            (anything-books-log ">>   pdfimages : %s -> %s" path org-file)
            (deferred:$
              ;;(deferred:process "pdfimages" "-j" "-f" "1" "-l" "1" path org-head)
              ;;(deferred:processc it "convert" "-resize" (format "%sx%s" anything-books-cache-pixel anything-books-cache-pixel) org-file org-file))))
              (deferred:process
                "evince-thumbnailer" "-s" anything-books-cache-pixel path org-file))))
        (anything-books-preview-progress it 2 4)
        (deferred:nextc it
          (lambda (err)
            (if (anything-books-file-exists-p org-file)
                (anything-books-copy-file-d nil org-file cache-file)
              (error err)))))))))

(defun anything-books-preview-image-get-d (path)
  "Translate a PDF file to PNG image data."
  (anything-books-log ">> anything-books-preview-image-get-d : %s" path)
  (let ((image (anything-books-preview-image-cache-get-mru path)))
    (cond
     (image
      (anything-books-log ">>   cache hit : %s" path)
      (deferred:succeed image))
     (t
      (lexical-let
          ((path path) 
           (org-file (expand-file-name "_preview_org-000.jpg" anything-books-preview-temp-dir))
           (resized-file (expand-file-name "_preview_resized.png" anything-books-preview-temp-dir))
           (win (anything-books-preview-get-preview-window)))
        (deferred:$
          (cc:semaphore-interrupt-all anything-books-preview-semaphore)
          (anything-books-preview-progress it 1 4)
          (anything-books-preview-image-create-d it path)
          (anything-books-preview-progress it 3 4)
          (deferred:nextc it
            (lambda (cache-file)
              (let* ((ww (* (window-width win) (frame-char-width)))
                     (wh (* (- (window-height win) 2) (frame-char-height))))
                (anything-books-log ">>   convert : %s -> %s" cache-file resized-file)
                (deferred:$
                  (deferred:process
                    "convert" "-resize" (format "%ix%i" ww wh)
                    cache-file (concat (file-name-extension resized-file) ":" resized-file))
                  (deferred:nextc it (lambda (x) resized-file))))))
          (deferred:nextc it
            (lambda (ifile)
              (anything-books-log ">>   add cache : %s " ifile)
              (clear-image-cache)
              (let ((img (create-image (anything-books-preview-load-image-data ifile) 'png t)))
                (anything-books-preview-image-cache-add path img)
                img)))
          (deferred:error it
            (lambda (e) (anything-books-log "Preview Error : %s" e)))
          (deferred:nextc it
            (lambda (x)
              (cc:semaphore-release anything-books-preview-semaphore)
              (cc:signal-send anything-books-anything-channel 'image-load-finish)
              (when (file-exists-p org-file) (ignore-errors (delete-file org-file)))
              (when (file-exists-p resized-file) (ignore-errors (delete-file resized-file)))
              (anything-books-log ">>   cleanup done : %s" path)
              x))))))))

(defun anything-books-preview-load-image-data (file)
  (let ((buf (find-file-noselect file t t)))
    (prog1 (with-current-buffer buf (buffer-string))
      (kill-buffer buf))))

(defvar anything-books-preview-window nil "[internal]")

(defun anything-books-preview-get-preview-window ()
  (let ((win (anything-window)))
    (unless anything-books-preview-window
      (setq anything-books-preview-window 
            (cond
             ((< (window-width win) (* 2 (window-height win)))
              (split-window win))
             (t
              (split-window win (/ (window-width win) 2) t))))
      (set-window-buffer 
       anything-books-preview-window 
       (anything-books-preview-buffer-init "No Image...")))
    anything-books-preview-window))

(defvar anything-books-preview-semaphore (cc:semaphore-create 1) "[internal]")

(defun anything-books-preview (title path)
  (lexical-let ((path path) (title title))
    (anything-books-preview-get-preview-window)
    (deferred:$
      (anything-books-preview-image-get-d path)
      (deferred:nextc it
        (lambda (img)
          (cc:signal-send anything-books-anything-channel 'show-image title path img)))
      (deferred:error it
        (lambda (e) (message "Preview Error : %s" e))))))

;;; anything command

(defun anything-books-preview-action (file)
  (let ((title (substring (file-name-nondirectory file) 0 -4)))
    (anything-books-preview title file)))

(defun anything-books-file-to-title (path)
  (substring (file-name-nondirectory path) 0 -4))

(defun anything-books-collect-files ()
  (loop for i in (directory-files (expand-file-name anything-books-books-dir))
        for f = (expand-file-name i anything-books-books-dir)
        if (and (file-regular-p f) (string-match ".pdf$" i))
        collect (cons (anything-books-file-to-title i) f)))

(defun anything-books-open-file (file)
  (deferred:process anything-books-open-command file)
  (format "PDF Opening : %s" (anything-books-file-to-title file)))

(defvar anything-books-source
  '((name . "PDF Books")
    (candidates . anything-books-collect-files)
    (action 
     ("Open" 
      . (lambda (x) (anything-books-open-file x)))
     ("Add Title to kill-ring" 
      . (lambda (x) (kill-new x))))
    (migemo)
    (persistent-action . anything-books-preview-action)))

(defadvice anything-move-selection-common (after anything-books-anything)
  (when (eq (anything-buffer-get) anything-buffer)
    (anything-execute-persistent-action)))

(defun anything-books-command ()
  (interactive)
  (setq anything-books-preview-window nil)
  (setq anything-books-preview-image-cache nil)
  (cc:semaphore-release-all anything-books-preview-semaphore)
  (ad-activate-regexp "anything-books-anything")
  (unwind-protect
      (anything anything-books-source)
    (ad-deactivate-regexp "anything-books-anything")))


;; (setq anything-books-debug t)
;; (eval-current-buffer)
;; (anything-books-command)

(provide 'anything-books)
;;; anything-books.el ends here
