# Anything Books

This program collects PDF files in your book directory and show them in the Anything interface with a preview image. You can select a PDF more efficiently.

## Installation:

This program is dependent on followings:

- anything.el (http://www.emacswiki.org/cgi-bin/wiki/download/anything.el)
- deferred.el (http://github.com/kiwanami/emacs-deferred/raw/master/deferred.el)
- concurrent.el (http://github.com/kiwanami/emacs-deferred/raw/master/concurrent.el)
- evince-thumbnailer, ImageMagick(convert)

If you have an another method to create a cover image from a PDF file, 
you can use it with some customize variables.

Put anything-books.el in your load-path, and add following code.

    (require 'anything-books)
    (setq abks:books-dir "your PDF library path!")
    (global-set-key (kbd "M-8") 'anything-books-command) ; key bind example


----
(C) 2010 SAKURAI Masashi All rights reserved. m.sakurai at kiwanami.net
