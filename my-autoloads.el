;;; my-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "my" "my.el" (0 0 0 0))
;;; Generated autoloads from my.el

(autoload 'my/sample-text-j "my" "\
Insert sample japanese text into the buffer.

\(fn)" t nil)

(autoload 'my/sample-text "my" "\
Insert sample english (er, latin) text into the buffer.

\(fn)" t nil)

(autoload 'my/excel-to-org-table "my" "\
Convert pasted excel region to org table

\(fn)" t nil)

(autoload 'my/space-etc "my" "\
Remove zenkaku space etc.

\(fn)" t nil)

(autoload 'my/insert-date-string "my" "\


\(fn Y M D)" t nil)

(autoload 'my/concat "my" "\
Concatenates a SEQUENCE of strings with a SEPARATOR

\(fn SEPARATOR &rest SEQUENCE)" nil nil)

(autoload 'my/org-export-to-pdf "my" "\
Export .org to pdf file using wkhtmltopdf.

\(fn)" t nil)

(autoload 'my/max-width-for-embedded-instagram "my" "\
Replace max-width: property of embedded instagram code to W px. Default is 300px.

\(fn W)" t nil)

(autoload 'my/set-faces-family "my" "\
Set the font family attribute to FAMILY and height to HEIGHT, of faces specified by APROPOS

\(fn APROPOS FAMILY HEIGHT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "my" '("my/")))

;;;***

(provide 'my-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; my-autoloads.el ends here
