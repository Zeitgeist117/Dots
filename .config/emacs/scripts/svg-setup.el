(use-package svg-tag-mode)
(setq svg-tag-tags
      '(
		(":TODO:" . ((lambda (tag) (svg-tag-make "TODO" :inverse t :face 'pastelpurple))))
        (":WIP:" . ((lambda (tag) (svg-tag-make "WIP" :inverse t :face 'pastelgreen))))
        (":DONE:" . ((lambda (tag) (svg-tag-make "DONE" :inverse t :face 'pastelblue))))
        (":NOTE:" . ((lambda (tag) (svg-tag-make "NOTE"))))
        ("SCHEDULED:" . ((lambda (tag) (svg-tag-make "SCHEDULED" :inverse t :face 'cobolt))))
        ("DEADLINE:" . ((lambda (tag) (svg-tag-make "DEADLINE" :inverse t :face 'cobolt))))
	("elisp" . ((lambda (tag) (svg-tag-make "elisp" :face 'cyan))))
        ("+BEGIN_SRC" . ((lambda (tag) (svg-tag-make "BEGIN" :inverse t :face 'cyan))))
        ("+END_SRC" . ((lambda (tag) (svg-tag-make "END" :face 'cyan))))
        ("+RESULTS:" . ((lambda (tag) (svg-tag-make "RESULTS" :inverse t :face 'cobolt))))
        ("+title" . ((lambda (tag) (svg-tag-make "TITLE" :face 'cobolt))))
        ("+date" . ((lambda (tag) (svg-tag-make "DATE" :face 'cobolt))))
        ("+author" . ((lambda (tag) (svg-tag-make "AUTHOR" :face 'cobolt))))
        ("+OPTIONS" . ((lambda (tag) (svg-tag-make "OPTIONS" :face 'cobolt))))
        ("+PROPERTY" . ((lambda (tag) (svg-tag-make "PROPERTY" :face 'cobolt))))
        ("+STARTUP" . ((lambda (tag) (svg-tag-make "STARTUP" :inverse t :face 'cobolt))))
        ("+BEGIN:" . ((lambda (tag) (svg-tag-make "BEGIN" :inverse t :face 'org-tag))))
        ("+CAPTION:" . ((lambda (tag) (svg-tag-make "CAPTION" :inverse t :face 'cobolt))))
        ("+NAME:" . ((lambda (tag) (svg-tag-make "NAME" :face 'cobolt))))
        ("+END:" . ((lambda (tag) (svg-tag-make "END" :inverse t :face 'org-warning))))
		("[X]" . ((lambda (tag) (svg-tag-make "[X]" :inverse t :face 'org-checkbox-statistics-done))))
        ("[-]" . ((lambda (tag) (svg-tag-make "[-]" :inverse t :face 'org-checkbox-statistics-todo))))
		
 ;; Citation of the form [cite:@Knuth:1984] 
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))
;;; Works for stuff like :XXX|YYY:
	("\\(:[A-Z]+\\)\|[a-zA-Z#0-9]+:" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :inverse t
                                                          :margin 0 :crop-right t))))        

        (":[A-Z]+\\(\|[a-zA-Z#0-9]+:\\)" . ((lambda (tag)
                                           (svg-tag-make tag :beg 1 :end -1
                                                         :margin 0 :crop-left t))))
	;; Active date (with or without day name, with or without time)
	;; <2023-04-03 Sun 17:45>
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s \\)%s>" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse t :crop-right t :margin 0 :face 'org-agenda-date))))
        (,(format "<%s \\(%s>\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse nil :crop-left t :margin 0 :face 'org-agenda-date))))
))



(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")
(defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

(defun svg-progress-percent (value)
  (save-match-data
   (svg-image (svg-lib-concat
               (svg-lib-progress-bar  (/ (string-to-number value) 100.0)
                                 nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
               (svg-lib-tag (concat value "%")
                            nil :stroke 0 :margin 0)) :ascent 'center)))

(defun svg-progress-count (value)
  (save-match-data
    (let* ((seq (split-string value "/"))           
           (count (if (stringp (car seq))
                      (float (string-to-number (car seq)))
                    0))
           (total (if (stringp (cadr seq))
                      (float (string-to-number (cadr seq)))
                    1000)))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center))))

(setq svg-tag-tags
      `(
        ;; Org tags
        (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
        (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
        
        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority 
                                            :beg 2 :end -1 :margin 0))))

        ;; TODO / DONE
        ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
        ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


        ;; Citation of the form [cite:@Knuth:1984] 
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))

        
        ;; Active date (with or without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s \\)%s>" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
        (,(format "<%s \\(%s>\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

        ;; Inactive date  (with or without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

        ;; ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))
        ))
  (defun org-agenda-show-svg ()
    (let* ((case-fold-search nil)
           (keywords (mapcar #'svg-tag--build-keywords svg-tag--active-tags))
           (keyword (car keywords)))
      (while keyword
        (save-excursion
          (while (re-search-forward (nth 0 keyword) nil t)
            (overlay-put (make-overlay
                          (match-beginning 0) (match-end 0))
                         'display  (nth 3 (eval (nth 2 keyword)))) ))
        (pop keywords)
        (setq keyword (car keywords)))))
  (add-hook 'org-agenda-finalize-hook #'org-agenda-show-svg)

(provide 'svg-setup)

(add-hook 'prog-mode-hook 'svg-tag-mode)
(add-hook 'org-mode-hook 'svg-tag-mode)
;; (add-hook 'dashboard-mode-hook 'svg-tag-mode)
