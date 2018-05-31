;;; scimax-utils.el --- Utility functions scimax cannot live without

;;; Commentary:
;;

;;; Code:

(require 's)
(require 'avy)
(require 'helm)
(require 'helm-for-files)
(require 'org-agenda)
;; (require 'emacs-keybinding-command-tooltip-mode)

;; * Hotspots
(defcustom scimax-user-hotspot-commands '()
  "A-list of hotspots to jump to in `hotspots'.
These are shortcut to commands.
\(\"label\" . command)"
  :group 'scimax)


(defcustom scimax-user-hotspot-locations '()
  "A-list of hotspot locations to jump to in  `hotspots'.
\(\"label\" . \"Path to file\").

These are like bookmarks."
  :group 'scimax)


;;;###autoload
(defun hotspots (arg)
  "Helm interface to hotspot locations.
This includes user defined
commands (`scimax-user-hotspot-commands'),
locations (`scimax-user-hotspot-locations'), org agenda files,
recent files and bookmarks. You can set a bookmark also."
  (interactive "P")
  (helm :sources `(((name . "Commands")
                    (candidates . ,scimax-user-hotspot-commands)
                    (action . (("Open" . (lambda (x) (funcall x))))))
                   ((name . "My Locations")
                    (candidates . ,scimax-user-hotspot-locations)
                    (action . (("Open" . (lambda (x) (find-file x))))))
                   ((name . "My org files")
                    (candidates . ,org-agenda-files)
                    (action . (("Open" . (lambda (x) (find-file x))))))
                   helm-source-recentf
                   helm-source-bookmarks
                   helm-source-bookmark-set)))


;;;###autoload
(defun explorer ()
  "Open Finder or Windows Explorer in the current directory."
  (interactive)
  (cond
   ((string= system-type "darwin")
    (shell-command (format "open -b com.apple.finder %s"
                           (if (buffer-file-name)
                               (file-name-directory (buffer-file-name))
                             "~/"))))
   ((string= system-type "windows-nt")
    (shell-command (format "explorer %s"
                           (replace-regexp-in-string
                            "/" "\\\\"
                            (if (buffer-file-name)
                                (file-name-directory (buffer-file-name))
                              (expand-file-name  "~/"))))))
   ((string= system-type "gnu/linux")
    (let ((process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. For example: with nautilus
    )))


(defalias 'finder 'explorer "Alias for `explorer'.")


;;;###autoload
(defun bash ()
  "Open a bash window."
  (interactive)
  (cond
   ((string= system-type "darwin")
    (shell-command
     (format "open -b com.apple.terminal \"%s\""
             (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               (expand-file-name default-directory)))))
   ((string= system-type "windows-nt")
    (shell-command "start \"\" \"%SYSTEMDRIVE%\\Program Files\\Git\\bin\\bash.exe\" --login &"))
   ((string= system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "xfce4-terminal"
                     (concat "--working-directory=" default-directory) )))))




;; ;;;###autoload
;; (add-to-list 'safe-local-eval-forms
;;              '(progn (require 'emacs-keybinding-command-tooltip-mode)
;;                      (emacs-keybinding-command-tooltip-mode +1)))


;;;###autoload
(defun scimax-help ()
  "Open the ‘scimax’ manual in org-mode."
  (interactive)
  (find-file (expand-file-name
              "scimax.org"
              scimax-dir)))


;;;###autoload
(defun scimax-info ()
  "Open the info manual."
  (info "(scimax)")
  (emacs-keybinding-command-tooltip-mode +1))



;; (global-set-key (kbd "M-<backspace>") 'backward-kill-sentence)


;; case on regions
;;;###autoload
(defun sentence-case-region (r1 r2)
  "Capitalize the word at point, and the first word of each
sentence in the region."
  (interactive "r")
  (save-excursion
    (goto-char r1)
    (capitalize-word 1)
    (while (< (point) r2)
      (forward-sentence)
      (capitalize-word 1))))


;;;###autoload
(defun avy-jump-to-sentence ()
  "Jump to a sentence with avy."
  (interactive)
  (avy-with my-jumper
    (avy--process
     (let (p
           (e (window-end)))
       (save-excursion
         (goto-char (window-start))
         (push (point) p)
         (while (< (point) e)
           (forward-sentence)
           (save-excursion
             (backward-sentence)
             (push (point) p)))
         (reverse p)))
     (avy--style-fn avy-style))))


;;;###autoload
(defun avy-jump-to-paragraph ()
  "Jump to a paragraph with avy."
  (interactive)
  (avy-with my-jumper
    (avy--process
     (let (p
           (e (window-end)))
       (save-excursion
         (goto-char (window-start))
         (push (point) p)
         (while (< (point) e)
           (forward-paragraph)
           (push (point) p))
         (reverse p)))
     (avy--style-fn avy-style))))



(defmacro with-no-new-buffers (&rest body)
  "Run BODY, and kill any new buffers created.
Returns whatever BODY would return."
  (let ((current-buffers (buffer-list)))
    `(prog1
         (progn
           ,@body)
       (mapc (lambda (buf)
               (unless (-contains? ',current-buffers buf)
                 (kill-buffer buf)))
             (buffer-list)))))


(defmacro f-string (fmt)
  "Like `s-format' but with format fields in it.
FMT is a string to be expanded against the current lexical
environment. It is like what is used in `s-lex-format', but has
an expanded syntax to allow format-strings. For example:
${user-full-name 20s} will be expanded to the current value of
the variable `user-full-name' in a field 20 characters wide.
  (let ((f (sqrt 5)))  (f-string \"${f 1.2f}\"))
  will render as: 2.24
This function is inspired by the f-strings in Python 3.6, which I
enjoy using a lot.

You can also try putting expressions in for formatting, e.g.:
 (let ((a 11)) (f-string \"The sqrt of ${a} is ${(sqrt a) 1.2f}.\"))
 will render as \"The sqrt of 11 is 3.32\".
"
  (let* ((matches (s-match-strings-all"${\\(?3:\\(?1:[^} ]+\\) *\\(?2:[^}]*\\)\\)}" fmt))
         (agetter (cl-loop
		               for (m0 m1 m2 m3) in matches
		               collect
		               `(cons ,m3
			                    ,(if (s-starts-with? "(" m3)
			                         ;; This means an expression is used
			                         (with-temp-buffer
				                         (insert m3)
				                         (goto-char (point-min))
				                         (let ((expr (read (current-buffer)))
				                               (fmt (s-trim (buffer-substring (point) (point-max)))))
				                           `(format
				                             (format "%%%s" (if (string= ,fmt "")
							                                          (if s-lex-value-as-lisp "S" "s")
						                                          ,fmt))
				                             ,expr)))

			                       `(format
			                         (format "%%%s" (if (string= ,m2 "")
						                                      (if s-lex-value-as-lisp "S" "s")
						                                    ,m2))
			                         (symbol-value (intern ,m1))))))))

    `(s-format ,fmt 'aget (list ,@agetter))))


(provide 'scimax-utils)

;;; scimax-utils.el ends here
