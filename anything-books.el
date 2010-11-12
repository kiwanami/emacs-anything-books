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

;; This program collects PDF files in your book directory and show
;; them in the Anything interface with a preview image. You can select
;; a PDF more efficiently.

;;; Installation:

;; This program is dependent on followings:
;; - anything.el (http://www.emacswiki.org/cgi-bin/wiki/download/anything.el)
;; - deferred.el (http://github.com/kiwanami/emacs-deferred/raw/master/deferred.el)
;; - concurrent.el (http://github.com/kiwanami/emacs-deferred/raw/master/concurrent.el)
;; - evince-thumbnailer, ImageMagick(convert)
;; If you have an another method to create a cover image from a PDF file, 
;; you can use it with some customize variables.

;; Put anything-books.el in your load-path, and add following code.

;; (require 'anything-books)
;; (setq abks:boos-dir "your PDF library path!")
;; (global-set-key (kbd "M-8") 'anything-books-command) ; key bind example
;; 

(require 'cl)
(require 'concurrent)

;;; Code:

(defvar akbs:books-dir nil)
(defvar akbs:cache-dir ".cache")
(defvar akbs:cache-pixel "600")
(defvar akbs:open-command "acroread")
(defvar akbs:preview-temp-dir "/tmp" "A directory to save a preview image temporally.")

(defvar akbs:cmd-copy "cp" "Copy command")
(defvar akbs:copy-by-command t "If non-nil, this program copies files by the external command asynchronously. If nil, this program uses Emacs copy function `copy-file' synchronously.")

(defvar akbs:mkcover-cmd '("evince-thumbnailer" "-s" size pdf jpeg))
(defvar akbs:convert-cmd '("convert" "-resize" size from to))



;;; for debug

(eval-and-compile
  (defvar akbs:debug nil "Debug output switch.")) ; debug
(defvar akbs:debug-count 0 "[internal] Debug output counter.") ; debug

(defmacro akbs:log (&rest args)
  (when akbs:debug
    `(progn 
       (with-current-buffer (get-buffer-create "*anything-books-debug*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "%5i %s\n" akbs:debug-count (format ,@args)))))
       (incf akbs:debug-count))))

(defun akbs:message-mark ()
  (interactive)
  (akbs:log "==================== mark ==== %s" 
            (format-time-string "%H:%M:%S" (current-time))))

(defun akbs:debug-report-semaphore ()
  (interactive)
  (message
   "Semaphore: preview permit: %s / waiting: %s"
   (cc:semaphore-permits akbs:preview-semaphore)
   (length (cc:semaphore-waiting-deferreds akbs:preview-semaphore))))



;;; Utility

(defun akbs:list-template (template-list data-alist)
  (loop for i in template-list
        collect
        (cond 
         ((symbolp i)
          (cdr (assq i data-alist)))
         (t i))))

(defun akbs:file-exists-p (file)
  (and (file-exists-p file)
       (< 0 (nth 7 (file-attributes file)))))

(defun akbs:fix-directory ()
  "Make a directory for cache files in the current directory which has visiting file."
  (let ((img-dir (expand-file-name akbs:cache-dir akbs:books-dir)))
    (unless (file-directory-p img-dir)
      (make-directory img-dir))
    (unless (file-directory-p img-dir)
      (error "Could not create a image directory."))
    img-dir))

(defun akbs:get-cache-path (path)
  (let ((file-head (file-name-sans-extension 
                    (file-name-nondirectory path))))
    (expand-file-name 
     (concat file-head ".jpg")
     (akbs:fix-directory))))

(defun akbs:get-convert-tmp-file ()
  (expand-file-name "_preview_org.jpg" akbs:preview-temp-dir))

(defun akbs:get-preview-tmp-file ()
  (expand-file-name "_preview_resized.png" akbs:preview-temp-dir))

(defun akbs:get-image-type (file)
  (let ((type (intern (file-name-extension file))))
    (cond 
     ((eq type 'jpg) 'jpeg)
     (t type))))

(defun akbs:copy-file-d (d from-path to-path)
  (unless d (setq d (deferred:next 'identity)))
  (akbs:log ">>   local copy : %s -> %s" from-path to-path)
  (lexical-let ((from-path from-path) (to-path to-path))
    (deferred:$
      (if akbs:copy-by-command
          (deferred:processc d akbs:cmd-copy from-path to-path)
        (deferred:nextc d
          (lambda (x) (ignore-errors (copy-file from-path to-path t t)))))
      (deferred:nextc it
        (lambda (x) 
          (unless (akbs:file-exists-p to-path)
            (error "Can not copy the file : %s -> %s" from-path to-path))
          to-path)))))



;;; anything preview

;;; Cache Control

(defvar akbs:preview-image-cache nil "[internal]")
(defvar akbs:preview-image-cache-num 10 "[internal]")

(defun akbs:preview-image-cache-get-mru (path)
  (let ((cached-pair (assoc path akbs:preview-image-cache)))
    (when cached-pair
      (setq akbs:preview-image-cache
            (cons cached-pair
                  (loop for i in akbs:preview-image-cache
                        for ipath = (car i)
                        with count = 1
                        unless (or (equal path ipath) 
                                   (<= akbs:preview-image-cache-num count))
                        collect (progn (incf count) i)))))
    (cdr cached-pair)))

(defun akbs:preview-image-cache-add (path image)
  (push (cons path image) akbs:preview-image-cache)
  image)

;;; Preview Buffer

(defvar akbs:anything-channel nil "[internal]")
(defconst akbs:preview-buffer " *akbs:preview*")

(defun akbs:preview-buffer-init (title)
  (let ((buf (get-buffer akbs:preview-buffer)))
    (unless buf
      (setq buf (get-buffer-create akbs:preview-buffer))
      (with-current-buffer buf
        (buffer-disable-undo buf)
        (set (make-local-variable 'preview-title) "")
        (set (make-local-variable 'preview-progress) "")
        (set (make-local-variable 'preview-count) 0))
      (unless akbs:anything-channel
        (setq akbs:anything-channel (cc:signal-channel 'akbs:anything)))
      (cc:signal-disconnect-all akbs:anything-channel)
      (loop for i in '((show-image . akbs:preview-buffer-on-show-image)
                       (progress . akbs:preview-buffer-on-show-progress)
                       (animation . akbs:preview-buffer-on-show-animation)
                       (image-load-start . akbs:preview-buffer-start-animation)
                       (image-load-finish . akbs:preview-buffer-stop-animation))
            for ev = (car i)
            for f = (cdr i)
            do (cc:signal-connect akbs:anything-channel ev f)))
    (cc:signal-send akbs:anything-channel 'show-image title nil nil)
    buf))

(defun akbs:preview-buffer-on-show-image (args)
  (with-current-buffer (get-buffer akbs:preview-buffer)
    (destructuring-bind (event (title path img)) args
      (akbs:log "%s %s" title path)
      (setq preview-title title
            preview-count 0
            preview-progress "")
      (akbs:preview-buffer-update-mode-line)
      (erase-buffer)
      (cond 
       (path
        (insert " ")
        (add-text-properties (point-min) (point-max) (list 'display img))
        (goto-char (point-min)))
       (t
        (insert "No image..."))))))

(defconst akbs:preview-mode-line-format "%s %5s %s") ; animation, progress, title

(defun akbs:preview-buffer-update-mode-line ()
  (let ((anm "-/|\\"))
    (setq mode-line-format 
          (format akbs:preview-mode-line-format
                  (char-to-string 
                   (aref anm (% preview-count (length anm))))
                  preview-progress preview-title)))
  (force-mode-line-update))

(defun akbs:preview-buffer-on-show-progress (args)
  (with-current-buffer (get-buffer akbs:preview-buffer)
    (destructuring-bind (event (progress)) args
      (setq preview-progress progress)
      (akbs:preview-buffer-update-mode-line))))

(defun akbs:preview-buffer-on-show-animation (buf)
  (with-current-buffer (get-buffer akbs:preview-buffer)
    (incf preview-count)
    (akbs:preview-buffer-update-mode-line)))


(defvar akbs:preview-buffer-thread nil "[internal]")

(defun akbs:preview-buffer-stop-animation ()
  (setq akbs:preview-buffer-thread nil))

(defun akbs:preview-buffer-start-animation ()
  (unless akbs:preview-buffer-thread
    (setq akbs:preview-buffer-thread t)
    (cc:thread 
     60 
     (while akbs:preview-buffer-thread
       (cc:signal-send akbs:anything-channel 'animation)))))


(defun akbs:preview-progress (d current total)
  (lexical-let
      ((progress
        (apply 'concat
               (loop for i from 1 to total
                     collect (if (<= i current) "O" ".")))))
    (deferred:nextc (or d (deferred:succeed))
      (lambda (x)
        (cc:signal-send akbs:anything-channel 'progress progress)
        x))))




(defun akbs:preview-image-create-d (d path)
  "Translate a PDF file to JPEG file and place the file at the cache directory."
  (akbs:log ">> akbs:preview-image-create-d : %s" path)
  (lexical-let
      ((path path) 
       (jpeg-file (akbs:get-convert-tmp-file))
       (cache-file (akbs:get-cache-path path)))
    (cond
     ((akbs:file-exists-p cache-file)
      (akbs:log ">>   cache file... : %s" cache-file)
      (deferred:nextc d
        (lambda (x) cache-file)))
     (t
      (akbs:log ">>   converting... : %s" path)
      (deferred:$ d
        (deferred:nextc it
          (lambda (x)
            (akbs:log ">>   mkcover : %s -> %s" path jpeg-file)
            (apply 'deferred:process 
                   (akbs:list-template 
                    akbs:mkcover-cmd
                    `((size . ,akbs:cache-pixel)
                      (pdf . ,path) 
                      (jpeg . ,jpeg-file))))))
        (akbs:preview-progress it 2 4)
        (deferred:nextc it
          (lambda (err)
            (if (akbs:file-exists-p jpeg-file)
                (akbs:copy-file-d nil jpeg-file cache-file)
              (error err)))))))))

(defun akbs:preview-image-convert-d(d path)
  (deferred:nextc d
    (lambda (cache-file)
      (let* ((win (akbs:preview-get-preview-window))
             (ww (* (window-width win) (frame-char-width)))
             (wh (* (- (window-height win) 2) (frame-char-height))))
        (lexical-let ((resized-file (akbs:get-preview-tmp-file)))
          (akbs:log ">>   convert : %s -> %s" cache-file resized-file)
          (deferred:$
            (apply 'deferred:process
                   (akbs:list-template
                    akbs:convert-cmd
                    `((size . ,(format "%ix%i" ww wh))
                      (from . ,cache-file) (to . ,resized-file))))
            (deferred:nextc it (lambda (x) resized-file))))))))

(defun akbs:preview-image-get-d (path)
  "Translate a PDF file to a small image of the first page."
  (akbs:log ">> akbs:preview-image-get-d : %s" path)
  (let ((image (akbs:preview-image-cache-get-mru path)))
    (cond
     (image
      (akbs:log ">>   cache hit : %s" path)
      (deferred:succeed image))
     (t
      (lexical-let ((path path))
        (deferred:$
          (cc:semaphore-interrupt-all akbs:preview-semaphore)
          (deferred:nextc it
            (lambda (x) (cc:signal-send akbs:anything-channel 'image-convert-start) nil))
          (akbs:preview-progress it 1 4)
          (akbs:preview-image-create-d it path)
          (akbs:preview-progress it 3 4)
          (akbs:preview-image-convert-d it path)
          (deferred:nextc it
            (lambda (ifile)
              (akbs:log ">>   add cache : %s " ifile)
              (clear-image-cache)
              (let ((img (create-image
                          (akbs:preview-load-image-data ifile) 
                          (akbs:get-image-type ifile) t)))
                (akbs:preview-image-cache-add path img)
                img)))
          (deferred:error it
            (lambda (e) (akbs:log "Preview Error : %s" e)))
          (deferred:nextc it
            (lambda (x)
              (cc:semaphore-release akbs:preview-semaphore)
              (cc:signal-send akbs:anything-channel 'image-convert-finish)
              (akbs:log ">>   cleanup done : %s" path)
              x))))))))

(defun akbs:preview-load-image-data (file)
  (let ((buf (find-file-noselect file t t)))
    (prog1 (with-current-buffer buf (buffer-string))
      (kill-buffer buf))))

(defvar akbs:preview-window nil "[internal]")

(defun akbs:preview-get-preview-window ()
  (let ((win (anything-window)))
    (unless akbs:preview-window
      (setq akbs:preview-window 
            (cond
             ((< (window-width win) (* 2 (window-height win)))
              (split-window win))
             (t
              (split-window win (/ (window-width win) 2) t))))
      (set-window-buffer 
       akbs:preview-window 
       (akbs:preview-buffer-init "No Image...")))
    akbs:preview-window))

(defvar akbs:preview-semaphore (cc:semaphore-create 1) "[internal]")

(defun akbs:preview (title path)
  (lexical-let ((path path) (title title))
    (akbs:preview-get-preview-window)
    (deferred:$
      (akbs:preview-image-get-d path)
      (deferred:nextc it
        (lambda (img)
          (cc:signal-send akbs:anything-channel 'show-image title path img)))
      (deferred:error it
        (lambda (e) (message "Preview Error : %s" e))))))



;;; anything command

(defun akbs:file-to-title (path)
  (substring (file-name-nondirectory path) 0 -4))

(defun akbs:preview-action (file)
  (let ((title (akbs:file-to-title file)))
    (akbs:preview title file)))

(defun akbs:collect-files () ; fix: subdirectories
  (loop for i in (directory-files (expand-file-name akbs:books-dir))
        for f = (expand-file-name i akbs:books-dir)
        if (and (file-regular-p f) (string-match ".pdf$" i))
        collect (cons (akbs:file-to-title i) f)))

(defun akbs:open-file (file)
  (deferred:process akbs:open-command file)
  (format "PDF Opening : %s" (akbs:file-to-title file)))

(defvar anything-books-source
  '((name . "PDF Books")
    (candidates . akbs:collect-files)
    (action 
     ("Open" 
      . (lambda (x) (akbs:open-file x)))
     ("Add Title to kill-ring" 
      . (lambda (x) (kill-new x))))
    (migemo)
    (persistent-action . akbs:preview-action)))

(defadvice anything-move-selection-common (after akbs:anything)
  (when (eq (anything-buffer-get) anything-buffer)
    (anything-execute-persistent-action)))
(ad-deactivate-regexp "akbs:anything")

(defun anything-books-command ()
  (interactive)
  (cond
   ((null akbs:books-dir)
    (message "Set your book dir: `akbs:books-dir'."))
   (t
    (setq akbs:preview-window nil
          akbs:preview-image-cache nil)
    (cc:semaphore-release-all akbs:preview-semaphore)
    (ad-activate-regexp "akbs:anything")
    (unwind-protect
        (anything anything-books-source)
      (progn
        (setq akbs:preview-image-cache nil
              akbs:preview-window nil)
        (ad-deactivate-regexp "akbs:anything"))))))

;; (setq akbs:debug t)
;; (eval-current-buffer)
;; (anything-books-command)

  (provide 'anything-books)
;;; anything-books.el ends here
