;;; anything-books.el --- Anything command for PDF books

;; Copyright (C) 2010, 2011  SAKURAI Masashi
;; Time-stamp: <2012-01-30 15:23:04 sakurai>

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Version: 1.3
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
;; (setq abks:books-dir "your PDF library path!")
;; (global-set-key (kbd "M-8") 'anything-books-command) ; key bind example


;;; Customize

;; ## Cache directory
;;
;; This program creates a cache directory for PDF cover images at the
;; PDF directory.  The directory name of the cache directory is the
;; value of `abks:cache-dir', the default is `.cache'.

;; ## PDF cover image
;;
;; The cover images for the PDF files are created by the program
;; `abks:mkcover-cmd', the default is `evince-thumbnailer' which is
;; default document browser of the GNOME desktop environment.  If you
;; have the ImageMagick and the GhostScript, you can use the command
;; `convert' to make a cover image. Note that `evince-thumbnailer'
;; works faster than `convert'.
;;
;; Mac users (Leopard or later)  use `qlmanager' to create a cover
;; image.
;;
;; The other programs can be also available, such as `pdfimages',
;; `pdf2png' and so on.

;;; History:

;; Revision 1.3  2012/01/30  sakurai
;; Improved:  `abks:books-dir' can be taken a list of paths.
;; Improved:  Fixed some typos (thx @tkf)
;; 
;; Revision 1.2  2011/02/03  sakurai
;; Bug fixed: Leaving temporary files in the working directory.
;; Improved:  `abks:list-template' for GhostScript arguments.
;; Improved:  Followed the latest deferred.el.
;; 
;; Revision 1.1  2010/11/26  sakurai
;; Bug fixed: Wrong file collection in sub-directories. (thx @nari3)
;; Bug fixed: Wrong JPEG file generated.
;; Improved:  Added qlmanager settings and framework. (thx @peccul)
;; Improved:  Extracted the action list `anything-books-actions'.
;;
;; Revision 1.0  2010/11/17  sakurai
;; Initial revision.

(require 'cl)
(require 'concurrent)

;;; Code:

(defvar abks:books-dir nil "A PDF directory as a string for searching. If a list of the PDF directory strings is given, the anything-books searches entries from those directories.")

(defvar abks:open-command "acroread" "A PDF viewer program.")

(defvar abks:cache-dir ".cache" "A directory name for the thumbnail images.")

;; for evince setting
(defvar abks:cache-pixel "600" "The image size of the thumbnail cache during converting PDF to PNG(JPEG).")
(defvar abks:mkcover-cmd-pdf-postfix nil "The post-fix string for converting PDF to PNG(JPEG). See the below GhostScript setting.")
(defvar abks:mkcover-cmd '("evince-thumbnailer" "-s" size pdf thum))
(defvar abks:mkcover-image-ext ".png")

;; for ImageMagick and GhostScript setting
;; (setq abks:cache-pixel "600x600")
;; (setq abks:mkcover-cmd-pdf-postfix "[0]")
;; (setq abks:mkcover-cmd '("convert" "-resize" size pdf cache))
;; (setq abks:mkcover-image-ext ".jpg")

;; for Mac (Quick Look) setting
;; (setq abks:cache-pixel "600")
;; (setq abks:mkcover-cmd-pdf-postfix "")
;; (setq abks:mkcover-cmd '("qlmanage" "-t" pdf "-s" size "-o" dir))
;; (setq abks:mkcover-image-ext ".png")

;; for Windows (GhostScript and ImageMagick) setting
;; [replace 'xxxx' to correct path on your machine]
;; (setq abks:preview-temp-dir "C:/temp")
;; (setq abks:mkcover-cmd '("C:/xxxxx/gs9.00/bin/gswin32c.exe" "-dSAFER" "-dBATCH" "-dNOPAUSE" "-sDEVICE=png16m" "-r50" "-dLastPage=1" (format "-sOutputFile=%s" (cdr (assq 'thum data-alist))) pdf))
;; (setq abks:convert-cmd '("C:/Program Files/ImageMagick-xxxxx/convert.exe" "-resize" size from to))
;; (setq abks:mkcover-image-ext ".png")
;; (setq abks:copy-by-command nil)
;; (setq abks:open-command "fiber") ; meadow
;; (setq abks:open-command '("CMD.exe" "/C" file)) ; Emacs23

;; for Windows (Mupad and GraphicsMagick) setting
;; ref: https://gist.github.com/915070  (thx @osamu2001)
;; [replace 'xxxx' to correct path on your machine]
;; (setq abks:mkcover-cmd '("C:/xxx/mupdf/pdfdraw" "-o" thum pdf "1"))
;; (setq abks:convert-cmd '("C:/xxx/GraphicsMagick-1.3.12-Q16/gm.exe" "convert" "-resize" size from to))
;; (see the above settings for other variables.)

(defvar abks:cmd-copy "cp" "Copy command")
(defvar abks:copy-by-command t "If non-nil, this program copies files by the external command asynchronously. If nil, this program uses Emacs copy function `copy-file' synchronously.")

(defvar abks:convert-cmd '("convert" "-resize" size from to))
(defvar abks:preview-temp-dir "/tmp" "A directory to save a preview image temporally.")




;;; for debug

(eval-and-compile
  (defvar abks:debug nil "Debug output switch.")) ; debug
(defvar abks:debug-count 0 "[internal] Debug output counter.") ; debug

(defmacro abks:log (&rest args)
  (when abks:debug
    `(progn
       (with-current-buffer (get-buffer-create "*anything-books-debug*")
         (save-excursion
           (goto-char (point-max))
           (insert (format "%5i %s\n" abks:debug-count (format ,@args)))))
       (incf abks:debug-count))))

(defun abks:message-mark ()
  (interactive)
  (abks:log "==================== mark ==== %s"
            (format-time-string "%H:%M:%S" (current-time))))

(defun abks:debug-report-semaphore ()
  (interactive)
  (message
   "Semaphore: preview permit: %s / waiting: %s"
   (cc:semaphore-permits abks:preview-semaphore)
   (length (cc:semaphore-waiting-deferreds abks:preview-semaphore))))



;;; Utility

(defun abks:list-template (template-list data-alist)
  (loop for i in template-list
        collect
        (cond
         ((symbolp i)
          (cdr (assq i data-alist)))
         ((listp i)
          (let ((str (eval i)))
            (abks:log ">>  template eval : %S" str)
            str))
         (t i))))

(defun abks:file-exists-p (file)
  (and (file-exists-p file)
       (< 0 (nth 7 (file-attributes file)))))

(defun abks:fix-directory (path)
  "Make a directory for cache files in the current directory which has visiting file."
  (let ((img-dir (expand-file-name abks:cache-dir (file-name-directory path))))
    (unless (file-directory-p img-dir)
      (make-directory img-dir))
    (unless (file-directory-p img-dir)
      (error "Could not create a image directory."))
    img-dir))

(defun abks:get-cache-path (path)
  (let ((file-head (file-name-sans-extension
                    (file-name-nondirectory path))))
    (expand-file-name
     (concat file-head abks:mkcover-image-ext)
     (abks:fix-directory path))))

(defun abks:get-cover-image-file (path)
  (expand-file-name
   (concat (file-name-nondirectory path) abks:mkcover-image-ext)
   abks:preview-temp-dir))

(defun abks:get-preview-tmp-file ()
  (expand-file-name 
   (concat "_preview_resized" abks:mkcover-image-ext)
   abks:preview-temp-dir))

(defun abks:get-image-type (file)
  (let ((type (intern (file-name-extension file))))
    (cond
     ((eq type 'jpg) 'jpeg)
     (t type))))

(defun abks:move-file-d (d from-path to-path)
  (unless d (setq d (deferred:succeed)))
  (abks:log ">>   local move : %s -> %s" from-path to-path)
  (lexical-let ((from-path from-path) (to-path to-path))
    (deferred:$
      (if abks:copy-by-command
          (deferred:processc d abks:cmd-copy from-path to-path)
        (deferred:nextc d
          (lambda (x) (ignore-errors (copy-file from-path to-path t t)))))
      (deferred:nextc it
        (lambda (x)
          (cond
           ((abks:file-exists-p to-path)
            (delete-file from-path))
           (t
            (error "Can not copy the file : %s -> %s" from-path to-path)))
          to-path)))))



;;; anything preview

;;; Cache Control

(defvar abks:preview-image-cache nil "[internal]")
(defvar abks:preview-image-cache-num 10 "[internal]")

(defun abks:preview-image-cache-get-mru (path)
  (let ((cached-pair (assoc path abks:preview-image-cache)))
    (when cached-pair
      (setq abks:preview-image-cache
            (cons cached-pair
                  (loop for i in abks:preview-image-cache
                        for ipath = (car i)
                        with count = 1
                        unless (or (equal path ipath)
                                   (<= abks:preview-image-cache-num count))
                        collect (progn (incf count) i)))))
    (cdr cached-pair)))

(defun abks:preview-image-cache-add (path image)
  (push (cons path image) abks:preview-image-cache)
  image)

;;; Preview Buffer

(defvar abks:anything-channel nil "[internal]")
(defconst abks:preview-buffer " *abks:preview*")

(defun abks:preview-buffer-init (title)
  (let ((buf (get-buffer abks:preview-buffer)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create abks:preview-buffer))
      (with-current-buffer buf
        (buffer-disable-undo buf)
        (set (make-local-variable 'preview-title) "")
        (set (make-local-variable 'preview-progress) "")
        (set (make-local-variable 'preview-count) 0))
      (unless abks:anything-channel
        (setq abks:anything-channel (cc:signal-channel 'abks:anything)))
      (cc:signal-disconnect-all abks:anything-channel)
      (loop for i in '((show-image . abks:preview-buffer-on-show-image)
                       (progress . abks:preview-buffer-on-show-progress)
                       (animation . abks:preview-buffer-on-show-animation)
                       (image-convert-start . abks:preview-buffer-start-animation)
                       (image-convert-finish . abks:preview-buffer-stop-animation))
            for ev = (car i)
            for f = (cdr i)
            do (cc:signal-connect abks:anything-channel ev f))
      (when abks:debug
        (cc:signal-connect abks:anything-channel
                           t (lambda (args) (abks:log "SIGNAL / %S" (car args))))))
    (cc:signal-send abks:anything-channel 'show-image title nil nil)
    buf))

(defun abks:preview-buffer-on-show-image (args)
  (with-current-buffer (get-buffer abks:preview-buffer)
    (destructuring-bind (event (title path img)) args
      (abks:log "%s %s" title path)
      (setq preview-title title
            preview-count 0
            preview-progress "")
      (abks:preview-buffer-update-mode-line)
      (erase-buffer)
      (cond
       (path
        (insert (propertize " " 'display `(space :align-to (+ center (-0.5 . ,img)))))
        (insert-image img)
        (let ((win (get-buffer-window abks:preview-buffer)))
          (when win (set-window-point win (1+ (point-min))))))
       (t
        (insert "No image..."))))))

(defconst abks:preview-mode-line-format "%s %5s %s") ; animation, progress, title

(defun abks:preview-buffer-update-mode-line ()
  (let ((anm "-/|\\"))
    (setq mode-line-format
          (format abks:preview-mode-line-format
                  (char-to-string
                   (aref anm (% preview-count (length anm))))
                  preview-progress preview-title)))
  (force-mode-line-update))

(defun abks:preview-buffer-on-show-progress (args)
  (with-current-buffer (get-buffer abks:preview-buffer)
    (destructuring-bind (event (progress)) args
      (setq preview-progress progress)
      (abks:preview-buffer-update-mode-line))))

(defun abks:preview-buffer-on-show-animation (buf)
  (with-current-buffer (get-buffer abks:preview-buffer)
    (incf preview-count)
    (abks:preview-buffer-update-mode-line)))


(defvar abks:preview-buffer-thread nil "[internal]")

(defun abks:preview-buffer-stop-animation ()
  (setq abks:preview-buffer-thread nil))

(defun abks:preview-buffer-start-animation ()
  (unless abks:preview-buffer-thread
    (setq abks:preview-buffer-thread t)
    (cc:thread
     60
     (while abks:preview-buffer-thread
       (cc:signal-send abks:anything-channel 'animation)))))


(defun abks:preview-progress (d current total)
  (lexical-let
      ((progress
        (apply 'concat
               (loop for i from 1 to total
                     collect (if (<= i current) "O" ".")))))
    (deferred:watch (or d (deferred:succeed))
      (lambda (x)
        (cc:signal-send abks:anything-channel 'progress progress)))))




(defun abks:preview-image-create-d (d path)
  "Translate a PDF file to thumbnail file and place the file at the cache directory."
  (abks:log ">> abks:preview-image-create-d : %s" path)
  (lexical-let
      ((path path)
       (cache-file (abks:get-cache-path path))
       (thum-file (abks:get-cover-image-file path)))
    (cond
     ((abks:file-exists-p cache-file)
      (abks:log ">>   cache file... : %s" cache-file)
      (deferred:nextc d
        (lambda (x) cache-file)))
     (t
      (abks:log ">>   converting... : %s" path)
      (deferred:$ d
        (deferred:nextc it
          (lambda (x)
            (abks:log ">>   mkcover : %s -> %s" path thum-file)
            (apply 'deferred:process
                   (abks:list-template
                    abks:mkcover-cmd
                    `((size . ,abks:cache-pixel)
                      (pdf . ,(concat path abks:mkcover-cmd-pdf-postfix))
                      (thum . ,thum-file) (jpeg . ,thum-file)
                      (dir . ,abks:preview-temp-dir))))))
        (abks:preview-progress it 2 4)
        (deferred:nextc it
          (lambda (err)
            (if (abks:file-exists-p thum-file)
                (abks:move-file-d nil thum-file cache-file)
              (error err)))))))))

(defun abks:preview-image-convert-d(d path)
  (deferred:nextc d
    (lambda (cache-file)
      (let* ((win (abks:preview-get-preview-window))
             (ww (* (window-width win) (frame-char-width)))
             (wh (* (- (window-height win) 2) (frame-char-height))))
        (lexical-let ((resized-file (abks:get-preview-tmp-file)))
          (abks:log ">>   convert : %s -> %s [%sx%s]" cache-file resized-file ww wh)
          (deferred:$
            (apply 'deferred:process
                   (abks:list-template
                    abks:convert-cmd
                    `((size . ,(format "%ix%i" ww wh))
                      (from . ,cache-file) (to . ,resized-file))))
            (deferred:nextc it (lambda (x) resized-file))))))))

(defun abks:preview-image-get-d (path)
  "Translate a PDF file to a small image of the first page."
  (abks:log ">> abks:preview-image-get-d : %s" path)
  (let ((image (abks:preview-image-cache-get-mru path)))
    (cond
     (image
      (abks:log ">>   cache hit : %s" path)
      (deferred:succeed image))
     (t
      (lexical-let ((path path))
        (deferred:$
          (cc:semaphore-interrupt-all abks:preview-semaphore)
          (deferred:watch it
            (lambda (x) (cc:signal-send abks:anything-channel 'image-convert-start)))
          (abks:preview-progress it 1 4)
          (abks:preview-image-create-d it path)
          (abks:preview-progress it 3 4)
          (abks:preview-image-convert-d it path)
          (deferred:nextc it
            (lambda (ifile)
              (abks:log ">>   add cache : %s " path)
              (clear-image-cache)
              (let ((img (create-image
                          (abks:preview-load-image-data ifile)
                          (abks:get-image-type ifile) t)))
                (abks:preview-image-cache-add path img)
                img)))
          (deferred:error it
            (lambda (e) (abks:log "Preview Error : %s" e)))
          (deferred:watch it
            (lambda (x)
              (cc:semaphore-release abks:preview-semaphore)
              (cc:signal-send abks:anything-channel 'image-convert-finish)
              (abks:log ">>   cleanup done : %s" path)))))))))

(defun abks:preview-load-image-data (file)
  (let ((buf (find-file-noselect file t t)))
    (prog1 (with-current-buffer buf (buffer-string))
      (kill-buffer buf))))

(defvar abks:preview-window nil "[internal]")

(defun abks:preview-init-preview-window ()
  (let ((win (anything-window)))
    (unless (and abks:preview-window
                 (window-live-p abks:preview-window))
      (setq abks:preview-window
            (cond
             ((< (window-width win) (* 2 (window-height win)))
              (split-window win))
             (t
              (split-window win (/ (window-width win) 2) t))))
      (set-window-buffer
       abks:preview-window
       (abks:preview-buffer-init "No Image...")))
    abks:preview-window))

(defun abks:preview-get-preview-window ()
  (and (window-live-p abks:preview-window)
       abks:preview-window))

(defvar abks:preview-semaphore (cc:semaphore-create 1) "[internal]")

(defun abks:preview (title path)
  (lexical-let ((path path) (title title))
    (deferred:$
      (abks:preview-image-get-d path)
      (deferred:watch it
        (lambda (img)
          (cc:signal-send abks:anything-channel 'show-image title path img)))
      (deferred:error it
        (lambda (e) (message "Preview Error : %s" e))))))



;;; anything command

(defun abks:file-to-title (path)
  (substring (file-name-nondirectory path) 0 -4))

(defvar abks:preview-action-last-title nil
  "[internal] Preventing the duplicate action invocation.")

(defun abks:preview-action (file)
  (unless abks:preview-window
      (abks:preview-init-preview-window))
  (let ((title (abks:file-to-title file)))
    (unless (equal title abks:preview-action-last-title)
      (setq abks:preview-action-last-title title)
      (abks:preview title file))))

(defun abks:collect-files-sort (a b)
  (string-lessp (car a) (car b)))

(defun abks:collect-files ()
  (cond
   ((stringp abks:books-dir)
    (abks:collect-files-rec abks:books-dir))
   ((listp abks:books-dir)
    (loop for dir in abks:books-dir
          append (abks:collect-files-rec dir)))
   (t (error "Unknown directory type : %s" abks:books-dir))))

(defun abks:collect-files-rec (&optional dir)
  (when (file-accessible-directory-p dir)
    (loop for i in (directory-files (expand-file-name dir))
          for f = (expand-file-name i dir)
          with lst = nil
          if (and (file-regular-p f) (string-match ".pdf$" i))
          do (push (cons (abks:file-to-title i) f) lst)
          if (and (file-directory-p f) (string-match "[^\\.]$" i))
          do (setq lst (append lst (abks:collect-files-rec f)))
          finally return (sort lst 'abks:collect-files-sort))))

(defun abks:open-file (file)
  (apply 'deferred:process 
    (if (stringp abks:open-command)
        (list abks:open-command file)
      (abks:list-template
       abks:open-command
       `((file . ,file)))))
  (format "PDF Opening : %s" (abks:file-to-title file)))

(defvar anything-books-actions
  '(("Open" 
     . (lambda (x) (abks:open-file x)))
    ("Add the book title to kill-ring" 
     . (lambda (x) (kill-new (abks:file-to-title x))))))

(defun anything-books-source-get ()
  `((name . "PDF Books")
    (candidates . abks:collect-files)
    (action . ,anything-books-actions)
    (migemo)
    (persistent-action . abks:preview-action)))

(defadvice anything-move-selection-common (after abks:anything)
  (when (eq (anything-buffer-get) anything-buffer)
    (anything-execute-persistent-action)))
(ad-deactivate-regexp "abks:anything")

(defun abks:command-startup ()
  (abks:log ">> startup...")
  (setq abks:preview-window nil
        abks:preview-image-cache nil
        abks:preview-action-last-title nil)
  (cc:semaphore-release-all abks:preview-semaphore)
  (ad-activate-regexp "abks:anything")
  (abks:log ">> startup finished."))

(defun abks:command-cleanup ()
  (abks:log ">> cleanup...")
  (loop for f in (list (abks:get-preview-tmp-file))
        if (file-exists-p f)
        do (ignore-errors (delete-file f)))
  (setq abks:preview-image-cache nil
        abks:preview-window nil)
  (let ((buf (get-buffer abks:preview-buffer)))
    (when (and buf (buffer-live-p buf))
      (kill-buffer buf)))
  (abks:preview-buffer-stop-animation)
  (cc:signal-disconnect-all abks:anything-channel)
  (ad-deactivate-regexp "abks:anything")
  (abks:log ">> cleanup finished %S." abks:anything-channel))

(defun anything-books-command ()
  (interactive)
  (cond
   ((null abks:books-dir)
    (message "Set your book dir: `abks:books-dir'."))
   (t
    (abks:command-startup)
    (unwind-protect
        (anything (anything-books-source-get))
      (abks:command-cleanup)))))

;; (setq abks:debug t)
;; (eval-current-buffer)
;; (anything-books-command)

(provide 'anything-books)
;;; anything-books.el ends here
