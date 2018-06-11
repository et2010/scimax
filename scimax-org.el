;;; scimax-org.el --- org-mode configuration for scimax	-*- lexical-binding:t ; -*-

;; Copyright (C) 2017-2018 John Kitchin

;; Author:
;; URL:
;; Keywords: org
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (avy "0.4.0") (ov "1.0.6"))

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

;;; Code:
(require 'avy)
(require 'f)
(require 'org)
(require 'ov)


;;;###autoload
(defun scimax-org-teleport (&optional arg)
  "Teleport the current heading to after a headline selected with avy.
With a prefix ARG move the headline to before the selected
headline. With a numeric prefix, set the headline level. If ARG
is positive, move after, and if negative, move before."
  (interactive "P")
  ;; Kill current headline
  (org-mark-subtree)
  (kill-region (region-beginning) (region-end))
  ;; Jump to a visible headline
  (avy-with avy-goto-line (avy--generic-jump "^\\*+" nil avy-style))
  (cond
   ;; Move before  and change headline level
   ((and (numberp arg) (> 0 arg))
    (save-excursion
      (yank))
    ;; arg is what we want, second is what we have
    ;; if n is positive, we need to demote (increase level)
    (let ((n (- (abs arg) (car (org-heading-components)))))
      (cl-loop for i from 1 to (abs n)
               do
               (if (> 0 n)
                   (org-promote-subtree)
                 (org-demote-subtree)))))
   ;; Move after and change level
   ((and (numberp arg) (< 0 arg))
    (org-mark-subtree)
    (goto-char (region-end))
    (when (eobp) (insert "\n"))
    (save-excursion
      (yank))
    ;; n is what we want and second is what we have
    ;; if n is positive, we need to demote
    (let ((n (- (abs arg) (car (org-heading-components)))))
      (cl-loop for i from 1 to (abs n)
               do
               (if (> 0 n) (org-promote-subtree)
                 (org-demote-subtree)))))

   ;; move to before selection
   ((equal arg '(4))
    (save-excursion
      (yank)))
   ;; move to after selection
   (t
    (org-mark-subtree)
    (goto-char (region-end))
    (when (eobp) (insert "\n"))
    (save-excursion
      (yank))))
  (outline-hide-leaves))


;; * Babel settings
(defun scimax-org-align-result-table ()
  "Align tables in the subtree."
  (save-restriction
    (save-excursion
      (unless (org-before-first-heading-p) (org-narrow-to-subtree))
      (org-element-map (org-element-parse-buffer) 'table
        (lambda (tbl)
          (goto-char (org-element-property :post-affiliated tbl))
          (org-table-align))))))

(add-hook 'org-babel-after-execute-hook 'scimax-org-align-result-table)


;; * Fragment overlays
(defun scimax-org-latex-fragment-tooltip (beg end image imagetype)
  "Add the fragment tooltip to the overlay and set click function to toggle it."
  (overlay-put (ov-at) 'help-echo
               (concat (buffer-substring beg end)
                       "\nmouse-1 to toggle."))
  (overlay-put (ov-at) 'local-map (let ((map (make-sparse-keymap)))
                                    (define-key map [mouse-1]
                                      `(lambda ()
                                         (interactive)
                                         (org-remove-latex-fragment-image-overlays ,beg ,end)))
                                    map)))
(advice-add 'org--format-latex-make-overlay :after 'scimax-org-latex-fragment-tooltip)


;; ** numbering latex equations
(defun scimax-org-renumber-environment (orig-func &rest args)
  "A function to inject numbers in LaTeX fragment previews."
  (let ((results '())
        (counter -1)
        (numberp))

    (setq results (loop for (begin .  env) in
                        (org-element-map (org-element-parse-buffer) 'latex-environment
                          (lambda (env)
                            (cons
                             (org-element-property :begin env)
                             (org-element-property :value env))))
                        collect
                        (cond
                         ((and (string-match "\\\\begin{equation}" env)
                               (not (string-match "\\\\tag{" env)))
                          (incf counter)
                          (cons begin counter))
                         ((string-match "\\\\begin{align}" env)
                          (prog2
                              (incf counter)
                              (cons begin counter)
                            (with-temp-buffer
                              (insert env)
                              (goto-char (point-min))
                              ;; \\ is used for a new line. Each one leads to a number
                              (incf counter (count-matches "\\\\$"))
                              ;; unless there are nonumbers.
                              (goto-char (point-min))
                              (decf counter (count-matches "\\nonumber")))))
                         (t
                          (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))

  (apply orig-func args))
(advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)



;; * Markup commands for org-mode
;; (loop for (type beginning-marker end-marker)
;;       in '((subscript "_{" "}")
;;            (superscript "^{" "}")
;;            (italics "/" "/")
;;            (bold "*" "*")
;;            (verbatim "=" "=")
;;            (code "~" "~")
;;            (underline "_" "_")
;;            (strikethrough "+" "+"))
;;       do
;;       (eval `(defun ,(intern (format "org-%s-region-or-point" type)) ()
;;                ,(format "%s the region, word or character at point"
;;                         (upcase (symbol-name type)))
;;                (interactive)
;;                (cond
;;                 ;; We have an active region we want to apply
;;                 ((region-active-p)
;;                  (let* ((bounds (list (region-beginning) (region-end)))
;;                         (start (apply 'min bounds))
;;                         (end (apply 'max bounds))
;;                         (lines))
;;                    (unless (memq ',type '(subscript superscript))
;;                      (save-excursion
;;                        (goto-char start)
;;                        (unless (looking-at " \\|\\<")
;;                          (backward-word)
;;                          (setq start (point)))
;;                        (goto-char end)
;;                        (unless (looking-at " \\|\>")
;;                          (forward-word)
;;                          (setq end (point)))))
;;                    (setq lines
;;                          (s-join "\n" (mapcar
;;                                        (lambda (s)
;;                                          (if (not (string= (s-trim s) ""))
;;                                              (concat ,beginning-marker
;;                                                      (s-trim s)
;;                                                      ,end-marker)
;;                                            s))
;;                                        (split-string
;;                                         (buffer-substring start end) "\n"))))
;;                    (setf (buffer-substring start end) lines)
;;                    (forward-char (length lines))))
;;                 ;; We are on a word with no region selected
;;                 ((thing-at-point 'word)
;;                  (cond
;;                   ;; beginning of a word
;;                   ((looking-back " " 1)
;;                    (insert ,beginning-marker)
;;                    (re-search-forward "\\>")
;;                    (insert ,end-marker))
;;                   ;; end of a word
;;                   ((looking-back "\\>" 1)
;;                    (insert ,(concat beginning-marker end-marker))
;;                    (backward-char ,(length end-marker)))
;;                   ;; not at start or end, so we just sub/sup the character at point
;;                   ((memq ',type '(subscript superscript))
;;                    (insert ,beginning-marker)
;;                    (forward-char ,(- (length beginning-marker) 1))
;;                    (insert ,end-marker))
;;                   ;; somewhere else in a word, and handled sub/sup. mark up the
;;                   ;; whole word.
;;                   (t
;;                    (re-search-backward "\\<")
;;                    (insert ,beginning-marker)
;;                    (re-search-forward "\\>")
;;                    (insert ,end-marker))))
;;                 ;; not at a word or region, insert markers and put point between
;;                 ;; them.
;;                 (t
;;                  (insert ,(concat beginning-marker end-marker))
;;                  (backward-char ,(length end-marker)))))))

;;;###autoload
(defun scimax-org-latex-math-region-or-point (&optional arg)
  "Wrap the selected region in latex math markup.
\(\) or $$ (with prefix ARG) or @@latex:@@ with double prefix.
Or insert those and put point in the middle to add an equation."
  (interactive "P")
  (let ((chars
         (cond
          ((null arg)
           '("\\(" . "\\)"))
          ((equal arg '(4))
           '("$" . "$"))
          ((equal arg '(16))
           '("@@latex:" . "@@")))))
    (if (region-active-p)
        (progn
          (goto-char (region-end))
          (insert (cdr chars))
          (goto-char (region-beginning))
          (insert (car chars)))
      (insert (concat  (car chars) (cdr chars)))
      (backward-char (length (cdr chars))))))

;;;###autoload
(defun scimax-org-helm-insert-org-entity ()
  "Helm interface to insert an entity from `org-entities'.
F1 inserts utf-8 character
F2 inserts entity code
F3 inserts LaTeX code (does not wrap in math-mode)
F4 inserts HTML code
F5 inserts the entity code."
  (interactive)
  (helm :sources
        (reverse
         (let ((sources '())
               toplevel
               secondlevel)
           (dolist (element (append
                             '("* User" "** User entities")
                             org-entities-user org-entities))
             (when (and (stringp element)
                        (s-starts-with? "* " element))
               (setq toplevel element))
             (when (and (stringp element)
                        (s-starts-with? "** " element))
               (setq secondlevel element)
               (add-to-list
                'sources
                `((name . ,(concat
                            toplevel
                            (replace-regexp-in-string
                             "\\*\\*" " - " secondlevel)))
                  (candidates . nil)
                  (action . (("insert utf-8 char" . (lambda (x)
                                                      (mapc (lambda (candidate)
                                                              (insert (nth 6 candidate)))
                                                            (helm-marked-candidates))))
                             ("insert org entity" . (lambda (x)
                                                      (mapc (lambda (candidate)
                                                              (insert
                                                               (concat "\\" (car candidate))))
                                                            (helm-marked-candidates))))
                             ("insert latex" . (lambda (x)
                                                 (mapc (lambda (candidate)
                                                         (insert (nth 1 candidate)))
                                                       (helm-marked-candidates))))
                             ("insert html" . (lambda (x)
                                                (mapc (lambda (candidate)
                                                        (insert (nth 3 candidate)))
                                                      (helm-marked-candidates))))
                             ("insert code" . (lambda (x)
                                                (mapc (lambda (candidate)
                                                        (insert (format "%S" candidate)))
                                                      (helm-marked-candidates)))))))))
             (when (and element (listp element))
               (setf (cdr (assoc 'candidates (car sources)))
                     (append
                      (cdr (assoc 'candidates (car sources)))
                      (list (cons
                             (format "%10s %s" (nth 6 element) element)
                             element))))))
           sources))))


;;;###autoload
(defun scimax-org-ivy-insert-org-entity ()
  "Insert an org-entity using ivy."
  (interactive)
  (ivy-read "Entity: " (cl-loop for element in (append org-entities org-entities-user)
                                when (not (stringp element))
                                collect
                                (cons
                                 (format "%20s | %20s | %20s | %s"
                                         (cl-first element) ;name
                                         (cl-second element) ; latex
                                         (cl-fourth element) ; html
                                         (cl-seventh element)) ;utf-8
                                 element))
            :require-match t
            :action '(1
                      ("u" (lambda (candidate)
                             (insert (cl-seventh (cdr candidate)))) "utf-8")
                      ("o" (lambda (candidate)
                             (insert "\\" (cl-first (cdr candidate)))) "org-entity")
                      ("l" (lambda (candidate)
                             (insert (cl-second (cdr candidate)))) "latex")
                      ("h" (lambda (candidate)
                             (insert (cl-fourth (cdr candidate)))) "html")
                      ("a" (lambda (candidate)
                             (insert (cl-fifth (cdr candidate)))) "ascii")
                      ("L" (lambda (candidate)
                             (insert (cl-sixth (cdr candidate))) "Latin-1")))))


;; * Define man link
;;;###autoload
(defun scimax-org-man-store-link ()
  "Store a link to a man page."
  (when (memq major-mode '(Man-mode woman-mode))
    (let* ((page (save-excursion
                   (goto-char (point-min))
                   (re-search-forward " ")
                   (buffer-substring (point-min) (point))))
           (link (concat "man:" page))
           (description (format "Manpage for %s" page)))
      (org-store-link-props
       :type "man"
       :link link
       :description description))))

;;;###autoload
(when (fboundp 'org-link-set-parameters)
  (org-link-set-parameters "man"
                           :follow (lambda (path) (man path))
                           :store 'scimax-org-man-store-link))


;; * Ivy navigation
;;;###autoload
(defun scimax-org-avy-jump-to-visible-headline ()
  "Jump to visible headline in the buffer."
  (interactive)
  (org-mark-ring-push)
  (avy-with avy-goto-line (avy--generic-jump "^\\*+" nil avy-style)))


;;;###autoload
(defun scimax-org-avy-jump-to-visible-sentence ()
  "Jump to visible sentence in the buffer."
  (interactive)
  (org-mark-ring-push)
  (avy-with avy-goto-line (avy--generic-jump (sentence-end) nil avy-style))
  (forward-sentence))

(defun ivy--regex-pinyin (str)
  (ivy--regex (pinyinlib-build-regexp-string str)))

;;;###autoload
(defun scimax-org-ivy-org-heading ()
  "Jump to heading in the current buffer."
  (interactive)
  (let ((headlines '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              ;; this matches org headings in elisp too.
              "^\\(;; \\)?\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ 	]*$"  nil t)
        (cl-pushnew (list
                     (format "%-80s"
                             (match-string 0))
                     (cons 'position (match-beginning 0)))
                    headlines)))
    (ivy-read "Headline: "
              (reverse headlines)
              :action (lambda (candidate)
                        (org-mark-ring-push)
                        (goto-char (cdr (assoc 'position candidate)))
                        (outline-show-entry))
              :re-builder #'ivy--regex-pinyin)))


;;;###autoload
(defun scimax-org-ivy-agenda-heading ()
  "Jump to a heading in an agenda file."
  (interactive)
  (let ((headlines '()))
    ;; these files should be open already since they are agenda files.
    (loop for file in (org-agenda-files) do
          (with-current-buffer (find-file-noselect file)
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward org-heading-regexp nil t)
                (cl-pushnew (list
                             (format "%-80s (%s)"
                                     (match-string 0)
                                     (file-name-nondirectory file))
                             :file file
                             :position (match-beginning 0))
                            headlines)))))
    (ivy-read "Headline: "
              (reverse headlines)
              :action (lambda (candidate)
                        (org-mark-ring-push)
                        (find-file (plist-get (cdr candidate) :file))
                        (goto-char (plist-get (cdr candidate) :position))
                        (outline-show-entry)))))


(defun scimax-org--ivy-heading-in-files (files &optional fontify)
  "Jump to org heading in FILES.
Optional FONTIFY colors the headlines. It might slow things down
a lot with large numbers of org-files or long org-files. This
function does not open the files."
  (let ((headlines '()))
    (loop for file in files do
          (with-temp-buffer
            (insert-file-contents file)
            (when fontify
              (org-mode)
              (font-lock-fontify-buffer))
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (cl-pushnew (list
                           (format "%-80s (%s)"
                                   (match-string 0)
                                   (file-name-nondirectory file))
                           :file file
                           :position (match-beginning 0))
                          headlines))))
    (ivy-read "Headline: "
              (reverse headlines)
              :action (lambda (candidate)
                        (org-mark-ring-push)
                        (find-file (plist-get (cdr candidate) :file))
                        (goto-char (plist-get (cdr candidate) :position))
                        (outline-show-entry)))))


;;;###autoload
(defun scimax-org-ivy-heading-in-directory (&optional recursive)
  "Jump to heading in an org file in the current directory.
Use a prefix arg to make it RECURSIVE.
Use a double prefix to make it recursive and fontified."
  (interactive "P")
  (let ((fontify nil))
    (when (equal recursive '(16))
      (setq fontify t))
    (scimax-org--ivy-heading-in-files
     (f-entries "."
                (lambda (f)
                  (and
                   (f-ext? f "org")
                   (not (s-contains? "#" f))))
                recursive)
     fontify)))


;;;###autoload
(defun scimax-org-ivy-project-headline (&optional fontify)
  "Jump to a headline in an org-file in the current project.
The project is defined by projectile. Use a prefix arg FONTIFY
for colored headlines."
  (interactive "P")
  (scimax-org--ivy-heading-in-files
   (mapcar
    (lambda (f) (expand-file-name f (projectile-project-root)))
    (-filter (lambda (f)
               (and
                (f-ext? f "org")
                (not (s-contains? "#" f))))
             (projectile-current-project-files)))
   fontify))


;;;###autoload
(defun scimax-org-ivy-open-headline (&optional fontify)
  "Jump to a headline in an open org-file.
Use a prefix arg FONTIFY for colored headlines."
  (interactive "P")
  (scimax-org--ivy-heading-in-files
   (mapcar 'buffer-file-name
           (-filter (lambda (b)
                      (-when-let (f (buffer-file-name b))
                        (f-ext? f "org")))
                    (buffer-list)))
   fontify))



;;* org-numbered headings
(defun scimax-org-overlay-numbered-headings ()
  "Put numbered overlays on the headings."
  (interactive)
  (loop for (p lv) in (let ((counters (copy-list '(0 0 0 0 0 0 0 0 0 0)))
                            (current-level 1)
                            last-level)
                        (mapcar (lambda (x)
                                  (list (car x)
                                        ;; trim trailing zeros
                                        (let ((v (nth 1 x)))
                                          (while (= 0 (car (last v)))
                                            (setq v (butlast v)))
                                          v)))
                                (org-map-entries
                                 (lambda ()
                                   (let* ((hl (org-element-context))
                                          (level (org-element-property :level hl)))
                                     (setq last-level current-level
                                           current-level level)
                                     (cond
                                      ;; no level change or increase, increment level counter
                                      ((or (= last-level current-level)
                                           (> current-level last-level))
                                       (incf (nth current-level counters)))

                                      ;; decrease in level
                                      (t
                                       (loop for i from (+ 1 current-level) below (length counters)
                                             do
                                             (setf (nth i counters) 0))
                                       (incf (nth current-level counters))))

                                     (list (point) (-slice counters 1)))))))
        do
        (let ((ov (make-overlay p p)))
          (overlay-put ov 'before-string (concat (mapconcat 'number-to-string lv ".") ". "))
          (overlay-put ov 'numbered-heading t))))


(define-minor-mode scimax-numbered-org-mode
  "Minor mode to number org headings."
  :init-value nil
  (cl-labels ((fl-noh (limit) (save-restriction
                                (widen)
                                (ov-clear 'numbered-heading)
                                (scimax-org-overlay-numbered-headings))))

    (if scimax-numbered-org-mode
        (progn
          (font-lock-add-keywords
           nil
           `((fl-noh 0 nil)))
          (font-lock-fontify-buffer))
      (ov-clear 'numbered-heading)
      (font-lock-remove-keywords
       nil
       `((fl-noh 0 nil))))))

(provide 'scimax-org)

;;; scimax-org.el ends here
