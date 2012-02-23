;; go-back.el
;; ------------------------------------------
;; author: Andreas Raster <lazor@affenbande.org>
;;
;; loosely based on and inspired by point-stack.el by
;; matt harrison (matthewharrison@gmail.com)
;; dmitry gutov  (dgutov@yandex.ru)


(defvar go-back-past '((nil (point-min) (point-min))))
(make-variable-buffer-local 'go-back-past)

(defvar go-back-future '((nil (point-max) (point-max))))
(make-variable-buffer-local 'go-back-future)

(defvar go-back-current '(nil (point-min) (point-min)))
(make-variable-buffer-local 'go-back-current)

(defvar go-back-history '())
(make-variable-buffer-local 'go-back-history)

(defvar go-back-last-jump-cost 0)
(make-variable-buffer-local 'go-back-last-jump-cost)


(setq go-back-past '((nil (point-min) (point-min)))
      go-back-future '((nil (point-max) (point-max)))
      go-back-current '(nil  (point-min) (point-min))
      go-back-history '()
      go-back-last-jump-cost 0)

(defun go-back-ignore-word-p (w &optional min-length)
  (and (< (length (substring-no-properties w)) (or min-length 2))
       (< (let ((hs (make-hash-table)))
            (mapcar (lambda (c) (puthash c 'found hs)) w)
            (hash-table-count hs)) 3)))

(defun go-back-ignore-line-p (&optional min-length)
  (save-excursion
    (beginning-of-line)
    (or (looking-at "^\\s-*$")
        (= (length (remove-if (lambda (w) (go-back-ignore-word-p w min-length))
                              (split-string (buffer-substring (point-at-bol) (point-at-eol)) " " t))) 0))))

(defun go-back-previous-line ()
  (interactive)
  (unless (save-excursion (beginning-of-line) (bobp))
    (previous-line-nomark)
    (let ((direction 'up))
      (while (go-back-ignore-line-p)
        (when (save-excursion
                (beginning-of-line)
                (bobp))
          (setq direction 'down))
        (if (eq direction 'up)
            (previous-line-nomark)
          (next-line-nomark))
        ))))

(defun go-back-next-line ()
  (interactive)
  (unless (save-excursion (end-of-line) (eobp))
    (next-line-nomark)
    (let ((direction 'down))
      (while (go-back-ignore-line-p)
        (when (save-excursion
                (end-of-line)
                (eobp))
          (setq direction 'up))
        (if (eq direction 'down)
            (next-line-nomark)
          (previous-line-nomark))
        ))))

(defun go-back-go (loc)
  (switch-to-buffer (or (nth 0 loc)
                        (current-buffer)))
  (set-window-start nil (eval (nth 2 loc)))
  (let* ((p (eval (nth 1 loc)))
         (re-left (nth 3 loc))
         (re-right (nth 4 loc)))
    (if (and (not (null re-left))
             (not (null re-right)))
        (let ((m1 (save-excursion
                    (unless (string-equal re-left "")
                      (goto-char (point-min))
                      (re-search-forward re-left nil t))))
              (m2 (save-excursion
                    (unless (string-equal re-right "")
                      (goto-char (point-max))
                      (re-search-backward re-right nil t))))
              (m3 (save-excursion
                    (unless (all (apply-partially 'eq ? ) (string-to-list re-left))
                      (goto-char p)
                      (goto-char (point-at-bol))
                      (re-search-forward (car (reverse (split-string re-left " " t))) nil t))))
              (m4 (save-excursion
                    (unless (all (apply-partially 'eq ? ) (string-to-list re-right))
                      (goto-char p)
                      (goto-char (point-at-eol))
                      (re-search-backward (car (reverse (split-string re-right " " t))) nil t)))))
          (print `(,m1 ,m2 ,m3 ,m4))
          (goto-char (or (car (sort (remove-if-not 'identity `(,m1 ,m2 ,m3 ,m4)) (lambda (a b) (< (abs (- p a)) (abs (- p b))))))
                         p)))
      (goto-char p))))


(defun go-back-make-location ()
  (save-excursion
    (when (go-back-ignore-line-p)
      (go-back-previous-line)
      (goto-char (point-at-eol)))
    (list (current-buffer)
          (point)
          (window-start)
          (regexp-quote (buffer-substring (point-at-bol) (point)))
          (regexp-quote (buffer-substring (point) (point-at-eol)))
          1)))

(defun go-back-dump-state ()
  (interactive)
  (let ((l go-back-past)
        (r go-back-future))
    (message (concat "["
                     (mapconcat #'prin1-to-string (mapcar #'second l) ",")
                     "],"
                     (prin1-to-string (second go-back-current))
                     ",["
                     (mapconcat #'prin1-to-string (mapcar #'second r) ",")
                     "] cost: " (prin1-to-string go-back-last-jump-cost)))))

;; '(1 2 3 4) 4 '() -right-> '(1 2 3) 3 '(4)
;; '(1 2 3 4) 4 '() -left--> '(2 3 4) 1 '(1)
(defun* go-back-shift (&optional (direction :left))
  (interactive)
  (case direction
    (:right
     (add-to-list 'go-back-future (car (last go-back-past)))
     (add-to-list 'go-back-history `(shift ,direction))
     (setq go-back-past (butlast go-back-past))
     (when (null go-back-past)
       (setq go-back-past (last go-back-future)
             go-back-future (butlast go-back-future)))
     (setq go-back-current (car (last go-back-past)))
     )
    (:left

     (add-to-list 'go-back-past (car go-back-future) t)
     (add-to-list 'go-back-history `(shift ,direction))
     (setq go-back-future (cdr go-back-future))
     (when (null go-back-future)
       (setq go-back-future (list (first go-back-past))
             go-back-past (cdr go-back-past))))
    (setq go-back-current (car go-back-future))
    ))

(defun go-back-loc-equal (a b)
  (or (and (eq (nth 0 a) (nth 0 b))
           (string-equal (nth 3 a) (nth 3 b))
           (string-equal (nth 4 a) (nth 4 b)))
      (and (eq (nth 0 a) (nth 0 b))
           (eq (nth 1 a) (nth 1 b)))))

(defun* go-back-push (&optional loc (direction :left))
  (interactive)
  (unless loc
    (setq loc (go-back-make-location)))
  (case direction
    (:right
     (setq go-back-future (remove-if (lambda (x) (go-back-loc-equal x loc)) go-back-future))
     (add-to-list 'go-back-future loc nil 'go-back-loc-equal)
     (add-to-list 'go-back-history `(insert ,direction))
     (setq go-back-current loc))
    (:left
     (setq go-back-past (remove-if (lambda (x) (go-back-loc-equal x loc)) go-back-past))
     (add-to-list 'go-back-past loc t 'go-back-loc-equal)
     (add-to-list 'go-back-history `(insert ,direction))
     (setq go-back-current loc)))
  )

(defun* go-back-remove (&optional (direction :left))
  (interactive)
  (case direction
    (:right
     (when go-back-future
       (add-to-list 'go-back-history `(remove ,direction ,(car go-back-future)))
       (when (eq (car go-back-future) go-back-current)
         (setq go-back-current (car (cdr go-back-future))))
       (setq go-back-future (cdr go-back-future))))
     (:left
      (when go-back-past
        (add-to-list 'go-back-history `(remove ,direction ,(car (last go-back-past))))
        (when (eq (car (last go-back-past)) go-back-current)
          (setq go-back-current (car (last (butlast go-back-past)))))
        (setq go-back-past (butlast go-back-past)))))
  )

;; (defun go-back-rewind ()
;;   (interactive)
;;   (when go-back-history
;;     (case (first (car go-back-history))
;;       ('shift
;;        (case (second (car go-back-history))
;;          (:left
;;           (go-back-shift :right))
;;          (:right
;;           (go-back-shift :left))))
;;       ('insert
;;        (case (second (car go-back-history))
;;          (:left
;;           (go-back-remove :right))
;;          (:right
;;           (go-back-remove :left))))
;;       ('remove
;;        (case (second (car go-back-history))
;;          (:left
;;           (go-back-push :left (third (car go-back-history))))
;;          (:right
;;           (go-back-push :right (third (car go-back-history)))))))
;;     (setq go-back-history (cdr go-back-history))))

(defun go-back-prev ()
  (interactive)
  (let* ((loc (go-back-make-location)))
    (case last-command
      ('go-back-prev
       (go-back-shift :right)
       (go-back-go (car (last go-back-past)))
       (setq go-back-current (car (last go-back-past)))
       )
      ('go-back-next
       (go-back-go (car (last go-back-past)))
       (setq go-back-current (car (last go-back-past)))
       )
      (t
       (go-back-go go-back-current)
       (when (eq go-back-current (car go-back-future))
         (go-back-shift :left))
       (go-back-push loc :right)
       (setq go-back-current (car (last go-back-past)))
       )))
  (go-back-dump-state)
  )

(defun go-back-next ()
  (interactive)
  (let* ((loc (go-back-make-location)))
    (case last-command
      ('go-back-next
       (go-back-shift :left)
       (go-back-go (car go-back-future))
       (setq go-back-current (car go-back-future))
       )
      ('go-back-prev
       (go-back-go (car go-back-future))
       (setq go-back-current (car go-back-future))
       )
      (t
       (setq go-back-current (car go-back-future))
       (go-back-go go-back-current)
       )
      ))
  (go-back-dump-state)
  )

(setq go-back-trigger-command-symbols '((isearch-mode
                                         isearch-forward
                                         isearch-repeat-forward
                                         isearch-backward
                                         isearch-repeat-backward
                                         isearch-resume
                                         isearch-printing-char
                                         isearch-abort
                                         isearch-exit
                                         isearch-other-control-char)
                                        (undo-tree-undo
                                         undo-tree-redo)
                                        (highlight-phrase)
                                        (highlight-symbol-at-point)
                                        (highlight-symbol-prev
                                         highlight-symbol-jump
                                         highlight-symbol-next
                                         highlight-symbol-jump)
                                        (sourcemarker-visit)
                                        (imenu
                                         imenu-many)
                                        (scroll-up
                                         scroll-up-mark
                                         scroll-up-nomark
                                         scroll-down
                                         scroll-down-mark
                                         scroll-down-nomark)
                                        (save-buffer
                                         switch-to-buffer)
                                        (eval-defun
                                         eval-last-sexp
                                         eval-buffer
                                         compile)))

(setq go-back-cursor-commands '((next-line
                                 next-line-mark
                                 next-line-nomark
                                 previous-line
                                 previous-line-mark
                                 previous-line-nomark
                                 backward-char-mark
                                 backward-char-nomark
                                 backward-word
                                 backward-word-mark
                                 backward-word-nomark
                                 backward-sexp
                                 backward-sexp-mark
                                 backward-sexp-nomark
                                 backward-sentence
                                 backward-paragraph
                                 backward-paragraph-mark
                                 backward-paragraph-nomark
                                 forward-char-mark
                                 forward-char-nomark
                                 forward-word
                                 forward-word-mark
                                 forward-word-nomark
                                 forward-sexp
                                 forward-sexp-mark
                                 forward-sexp-nomark
                                 forward-sentence
                                 forward-paragraph
                                 forward-paragraph-mark
                                 forward-paragraph-nomark
                                 end-of-line
                                 end-of-line-mark
                                 end-of-line-nomark
                                 beginning-of-line
                                 beginning-of-line-mark
                                 beginning-of-line-nomark)))

;; (keyboard-quit)
;; (indent-for-tab-command)
;; (newline-and-indent)
;; (self-insert-command)
;; (backward-delete-char
;;  backward-delete-char-untabify)
;; (kill-region
;;  kill-whole-line-indent)

(defun go-back-pre-command-trigger ()
  (let* ((tc this-command)
         (lc last-command)
         (triggered nil))
    (unless (or (eq tc 'go-back-next)
                (eq tc 'go-back-prev)
                (eq tc 'go-back-push))
      ;; (loop for xs in go-back-cursor-commands
      ;;       until (when (some 'identity (mapcar (apply-partially 'eq tc) xs))
      ;;               (setq go-back-current (go-back-make-location))))
      (loop for ys in go-back-trigger-command-symbols
            until (when (some 'identity (mapcar (apply-partially 'eq tc) ys))
                    (setq triggered `(command ,ys))))
      (when triggered
        (unless (some 'identity (mapcar (apply-partially 'eq lc) (second triggered)))
          (unless (or (eq lc 'go-back-next)
                      (eq lc 'go-back-prev)
                      (eq lc 'go-back-push))
            (when (buffer-file-name (current-buffer))
              (go-back-push)
              )))))))


(eval-after-load "go-back"
  (add-hook 'pre-command-hook 'go-back-pre-command-trigger))

;;(remove-hook 'pre-command-hook 'go-back-pre-command-trigger)

(provide 'go-back)