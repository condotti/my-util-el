;;; my.el --- My own utility functions               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  

;; Author:  <akondo21@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;;###autoload
(defun my/sample-text-j nil
  "Insert sample japanese text into the buffer."
  (interactive
   (insert
    "月日は百代の過客にして、行かふ年も又旅人也。舟の上に生涯をうかべ、"
    "馬の口とらえて老をむかふる物は、日々旅にして旅を栖とす。古人も多く"
    "旅に死せるあり。予もいづれの年よりか、片雲の風にさそはれて、漂泊の"
    "思ひやまず、海浜にさすらへ、去年の秋江上の破屋に蜘の古巣をはらひて、"
    "やゝ年も暮、春立る霞の空に白川の関こえんと、そゞろ神の物につきて心"
    "をくるはせ、道祖神のまねきにあひて、取もの手につかず。もゝ引の破を"
    "つゞり、笠の緒付かえて、三里に灸すゆるより、松島の月先心にかゝりて、"
    "住る方は人に譲り、杉風が別墅に移るに、\n\n"
    
    "  草の戸も住替る代ぞひなの家\n\n"
    
    "面八句を庵の柱に懸置。")))

;;;###autoload
(defun my/sample-text nil
  "Insert sample english (er, latin) text into the buffer."
  (interactive
   (insert
    "Lorem ipsum dolor sit amet, illud bonorum id mei, at duo paulo regione. "
    "Agam scaevola interesset eam cu, est in maiorum abhorreant assueverit, "
    "vero quaeque reprimique ad nam. Mundi nonumy no vix, in eam noster sanctus "
    "dissentias, ne sed denique deterruisset. Eam vero viderer delicata id, "
    "voluptatum eloquentiam ex his, per everti denique ne. Virtute philosophia "
    "usu et, qui dicta feugiat tibique eu. Nec ad populo possit singulis, ei his "
    "falli definitionem. Solet delicata pro ad.\n\n"
    
    "Id mutat periculis nec, nisl aliquando gloriatur qui ne. An pro dolor "
    "laoreet, at nec placerat corrumpit. Simul malorum adversarium mea te, "
    "enim rebum ea vim, ut prima inermis explicari nam. Mel perpetua oportere "
    "similique ea.\n\n"
    
    "Eum eu purto tota aeque, est at illum clita splendide, cu alia purto cibo "
    "vel. Cu duo tale vocent intellegat, animal facilis ea vis, dicant numquam "
    "facilisis et mea. Modo duis summo an per, an sed veniam labores graecis. "
    "Quando libris no est, et usu malis aperiam, ad novum accusata mea. Animal "
    "definitionem ut nam, eam no vidit accumsan. Ea nam vitae numquam.\n\n"
    
    "Doctus sadipscing appellantur in eum, dolor corrumpit conclusionemque sit ex, "
    "mea graeco perpetua id. Has meis graece ad. Dolore consequuntur ne vix, "
    "nec eirmod audiam definitiones ex. Vel ei invenire intellegat scribentur.\n\n"
    
    "Qui wisi dictas periculis ut, eu qui ferri viderer corrumpit. Eam no solet "
    "prompta veritus, fabulas ullamcorper vis eu, ei vis probo erroribus. Modus "
    "debitis sed ei. Has dolorem constituam eu, quo te omnis impetus.")))

;;;###autoload
(defun my/excel-to-org-table ()
  "Convert pasted excel region to org table"
  (interactive)
  (save-excursion
    (mapc #'(lambda (lst)
              (replace-regexp (car lst) (cdr lst) nil (region-beginning) (region-end)))
          '(("^" . "|")
            ("\t" . "|")
            ("#[^!]+!" . "")))))

;;;###autoload
(defun my/space-etc ()
  "Remove zenkaku space etc."
  (interactive)
  (save-excursion
    (save-restriction
      (mapc #'(lambda (lst)
                (progn
                  (goto-char (point-min))
                  (while (re-search-forward (car lst) nil t)
                    (replace-match (cdr lst)))))
            '(("　" . " ")
              ("◎" . " ")
              ("\t" . " ")
              ("（" . "(")
              ("）" . ")")
              )))))

(defun my/html-body ()
  "To paste as a html fragment, copy the content of the body tag to paste buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((beg (progn
                   (goto-char (point-min))
                   (search-forward-regexp "<body.*>")
                   (point)))
            (end (progn
                   (goto-char (point-max))
                   (search-backward-regexp "</body>")
                   (point))))
        (copy-region-as-kill beg end)))))

;;;###autoload
(defun my/insert-date-string (y m d)
  (interactive "nyyyy: \nnmm: \nndd: ")
  (insert (format-time-string "%m/%d(%a) "
                              (encode-time 0 0 0 d m y))))

;;;###autoload
(defun my/concat (separator &rest sequence)
  "Concatenates a SEQUENCE of strings with a SEPARATOR"
  (mapconcat #'identity sequence separator))

(setq my-htmltopdf-program "wkhtmltopdf"
      my-htmltopdf-args (my/concat " "
                                   "--header-left [doctitle]"
                                   "--footer-center [page]/[toPage]"
                                   "--header-line --footer-line"
                                   "--header-right [date]"
                                   "--header-font-size 10"
                                   "--footer-font-size 10"
                                   "--margin-top 20"
                                   "--header-spacing 2"
                                   "--footer-spacing 2"
                                   "--disable-smart-shrinking"
                                   "--print-media-type"
                                   "--no-outline"))
(defun my/adoc-export (arg)
  "Convert asciidoc file to a html or a pdf by using asciidoctor.
Default to a pdf, or a html if ARG is not nil."
  (interactive "p")
  (save-excursion
    (when (eq major-mode 'adoc-mode)
      (let* ((pdf-file (concat (file-name-sans-extension buffer-file-name) ".pdf"))
             (html-file (concat (file-name-sans-extension buffer-file-name) ".html")))
        (if (= arg 1)
            (progn
              (shell-command (my/concat " " my/adoc-to-html-program my/adoc-to-html-args buffer-file-name))
              (eww-open-file html-file)) ; View with eww
          (progn
            (shell-command (my/concat " " my/adoc-to-pdf-program my/adoc-to-pdf-args buffer-file-name))
            (find-file pdf-file)))))    ; View with docview
    )
  )

(setq my/adoc-to-pdf-program (if (eq system-type 'windows-nt)
                                 (concat "d:/USER/Program/cygwin/bin/ruby.exe "
                                         (expand-file-name "~/bin/asciidoctor-pdf"))
                               "asciidoctor-pdf")
      my/adoc-to-pdf-args "-r asciidoctor-pdf-cjk"
      my/adoc-to-html-program (if (eq system-type 'windows-nt)
                                  (concat "d:/USER/Program/cygwin/bin/ruby.exe "
                                          (expand-file-name "~/bin/asciidoctor"))
                                "asciidoctor")
      my/adoc-to-html-args "")

(defun my/ido-recentf-open nil
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting...")))

(defun my/clojure-cheatsheet nil
  (interactive)
  (eww "http://clojure.org/cheatsheet"))

(defun my/dummy-function (&optional arg)
  (message "dummy"))

(defun my/wm-running? nil
  (thread-first "wmctrl -m; echo -n $?"
    (shell-command-to-string)
    (split-string "\n")
    (last)
    (car)
    (string-to-number)
    (zerop)))

;;;###autoload
(defun my/max-width-for-embedded-instagram (w)
  "Replace max-width: property of embedded instagram code to W px. Default is 300px."
  (interactive "p")
  (save-excursion
    (let ((new-prop (format "max-width:%dpx" (if (= w 1) 300 w))))
      (goto-char (point-min))
      (while (re-search-forward "max-width: *[0-9]+px" nil t)
        (replace-match new-prop nil nil)))))

;; ----------------------------------------------------------------------
;; Enables ime when startup (send F10 twice) by powershell.exe
;; ----------------------------------------------------------------------
(defun my/enable-ime nil
  "Sends F10 twice by powershell.exe"
  (let* ((f10 "[System.Windows.Forms.SendKeys]::SendWait(\"\\\"{F10}\\\"\")")
         (script (concat "\"" (my/concat "; "
                                         "Add-Type -AssemblyName System.Windows.Forms"
                                         "Start-Sleep 1"
                                         f10
                                         "Start-Sleep 1"
                                         f10
                                         "Exit")
                         "\""))
         (command (my/concat " " "powershell.exe" "-Command" script)))
    (shell-command-to-string command)))
;; (when (eq system-type 'windows-nt)
;;   (add-hook 'window-setup-hook #'my/enable-ime))

;; ----------------------------------------------------------------------
;; Fix font family of faces
;; ----------------------------------------------------------------------
;;;###autoload
(defun my/set-faces-attribute (regexp &rest attributes)
  "Set ATTRIBUTES to the faces specified by REGEXP"
  (dolist (face (face-list))
    (when (string-match regexp (symbol-name face))
      (apply #'set-face-attribute (append (list face nil) attributes)))))

;; ----------------------------------------------------------------------
;; make date noun to date string conversion dictionary
;; ----------------------------------------------------------------------
(defvar my/date-dic-file "~/.anthy/imported_words_default.d/date.dic")
(defvar my/make-date-dic-interval 300)

;;;###autoload
(defun my/make-date-dic ()
  "Create date transformation dictionary for anthy"
  (when (or (not (file-exists-p my/date-dic-file))
	  (string< (thread-last my/date-dic-file
		     (file-attributes)
		     (file-attribute-modification-time)
		     (format-time-string "%F"))
		   (format-time-string "%F")))
      (let ((buf (find-file-noselect my/date-dic-file t)))
	(switch-to-buffer buf)
	(set-buffer-file-coding-system 'utf-8-unix)
	(erase-buffer)
	(cl-loop for (noun . offset) in '(("さきおととい" . -3)
					  ("おととい" . -2)
					  ("きのう" . -1)
					  ("きょう" . 0)
					  ("あす" . 1)
					  ("あさって" . 2)
					  ("しあさって". 3))
		 do
		 (dolist (fmt '("%Y/%-m/%-d" "%F" "%Y%m%d"))
		   (insert noun
			   " #T35*500 "
			   (format-time-string fmt
					       (time-add (current-time)
							 (seconds-to-time (* offset 24 60 60))))
			   "\n")))
	(sort-lines nil (point-min) (point-max))
	(save-buffer)
	(kill-buffer buf))))

;; Run this once and repeat periodically with idle timer
;; (my/make-date-dic)
;; (defvar my/make-date-dic-timer
;;   (run-with-idle-timer my/make-date-dic-interval t #'my/make-date-dic))

;; ----------------------------------------------------------------------
;; insert week plan table
;; ----------------------------------------------------------------------
;;;###autoload
(defun my/insert-week-table (date-str)
  "Insert week plan as a table in org or markdown"
  (interactive "sYYYYMMDD: ")
  (let* ((date (string-to-number (replace-regexp-in-string "[^0-9]" "" date-str)))
	 (year (/ date 10000))
	 (month (mod (/ date 100) 100))
	 (day (mod date 100))
	 (date-in-sec (time-to-seconds (encode-time 0 0 0 day month year))))
    (save-excursion
      (dotimes (n 7)
	(thread-last date-in-sec
	  (+ (* n 24 60 60))
	  (seconds-to-time)
	  (format-time-string "| %-m/%-d(%a) |\n")
	  (insert))))))

;; ----------------------------------------------------------------------
;; insert gist-it html tags
;; ----------------------------------------------------------------------
;;;###autoload
(defun my/gist-it (url)
  "Insert gist-it html tags"
  (interactive "sGithub URL: ")
  (save-excursion
    (insert (format "<script src=\"https://gist-it.appspot.com/%s\"></script>\n" url))))

;; ----------------------------------------------------------------------
;; convert html to pdf (refactored version of my/(markdown|org)-export-to-pdf
;; ----------------------------------------------------------------------
(defun my/export-to-pdf (fn modes)
  "Convert org, markdown etc. to pdf by using FN to convert html when major mode is one of MODES."
  
  (when (member major-mode modes)
    (let* ((htmlfile (funcall fn))
	   (pdffile (concat (file-name-sans-extension htmlfile) ".pdf")))
      (thread-last pdffile
	(file-name-nondirectory)
	(my/concat " " my-htmltopdf-program my-htmltopdf-args (expand-file-name htmlfile))
	(shell-command))
      (find-file pdffile))))

;;;###autoload
(defun my/markdown-export-to-pdf nil
  (interactive)
  (save-excursion
    (my/export-to-pdf #'markdown-export '(markdown-mode gfm-mode))))

;;;###autoload
(defun my/org-export-to-pdf nil
  (interactive)
  (save-excursion
    (my/export-to-pdf #'org-html-export-to-html '(org-mode))))

(provide 'my)
;;; my.el ends here
