;; go-back.el -- continue working on files from where you saved
;; Copyright (C) 2012 Andreas Raster <lazor@affenbande.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; loosely based on and inspired by point-stack.el by
;; matt harrison (matthewharrison@gmail.com)
;; dmitry gutov  (dgutov@yandex.ru)

(require 'cl)

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
                              (split-string (buffer-substring-no-properties (point-at-bol) (point-at-eol)) " " t))) 0))))

(defun go-back-previous-line ()
  (interactive)
  (unless (save-excursion (beginning-of-line) (bobp))
    (previous-line)
    (let ((direction 'up))
      (while (go-back-ignore-line-p)
        (when (save-excursion
                (beginning-of-line)
                (bobp))
          (setq direction 'down))
        (if (eq direction 'up)
            (previous-line)
          (next-line))
        ))))

(defun go-back-next-line ()
  (interactive)
  (unless (save-excursion (end-of-line) (eobp))
    (next-line)
    (let ((direction 'down))
      (while (go-back-ignore-line-p)
        (when (save-excursion
                (end-of-line)
                (eobp))
          (setq direction 'up))
        (if (eq direction 'down)
            (next-line)
          (previous-line))
        ))))

(defun go-back-go (loc)
  (flet ((line-to-re (line)
                     (when line
                       (let ((words (remove-if 'go-back-ignore-word-p
                                               (split-string line " " t)))
                             (res '()))
                         (while words
                           (cond ((null res)
                                  (add-to-list 'res (regexp-quote (concat (pop words) " "))))
                                 ((null (cdr words))
                                  (add-to-list 'res (regexp-quote (concat " " (pop words)))))
                                 (t
                                  (add-to-list 'res (regexp-quote (concat " " (pop words) " "))))))
                         res))))
    (switch-to-buffer (or (nth 0 loc)
                          (current-buffer)))
    (set-window-start nil (eval (nth 2 loc)))
    (let* ((p (eval (nth 1 loc)))
           (line-left (nth 3 loc))
           (line-right (nth 4 loc))
           (re-left (regexp-quote line-left))
           (re-right (regexp-quote line-right))
           (line-prev (nth 5 loc))
           (line-next (nth 6 loc))
           (words-1 (line-to-re (concat line-left line-right)))
           (words-2 (line-to-re line-prev))
           (words-3 (line-to-re line-next)))
      ;; find every line with a matching word, normalize, use line with most matches
      (let ((matches-1 (save-excursion
                         (loop for w1 in words-1
                               if (string-equal w1 "#bobp#")
                               collect '(0)
                               else
                               if (string-equal w1 "#eobp#")
                               collect `(,(point-max))
                               else
                               do (goto-char (point-min))
                               collect (loop while (re-search-forward w1 nil t)
                                             collect (point-at-bol)))))
            (matches-2 (save-excursion
                         (loop for w2 in words-2
                               if (string-equal w2 "#bobp#")
                               collect '(0)
                               else
                               if (string-equal w2 "#eobp#")
                               collect `(,(point-max))
                               else
                               do (goto-char (point-min))
                               collect (loop while (re-search-forward w2 nil t)
                                             collect (point-at-bol 2)))))
            (matches-3 (save-excursion
                         (loop for w3 in words-3
                               if (string-equal w3 "#bobp#")
                               collect '(0)
                               else
                               if (string-equal w3 "#eobp#")
                               collect `(,(point-max))
                               else
                               do (goto-char (point-min))
                               collect (loop while (re-search-forward w3 nil t)
                                             collect (point-at-bol 0))))))
        (let* ((all-matches (sort (append (apply 'append matches-1)
                                          (apply 'append matches-2)
                                          (apply 'append matches-3))
                                  (lambda (a b)
                                    (< a b))))
               (last-match (car all-matches))
               (counter 0)
               (longest-counter 0)
               (longest-matches `(,last-match))
               (closest-diff nil)
               (closest-match nil)
               )
          (dolist (current-match all-matches)
            (if (eq current-match last-match)
                (setq counter (1+ counter))
              (when (eq counter longest-counter)
                (setq longest-matches (append longest-matches `(,last-match))))
              (when (> counter longest-counter)
                (setq longest-counter counter
                      longest-matches `(,last-match)))
              (setq last-match current-match
                    counter 1)))
          (dolist (current-match longest-matches)
            (when (or (not closest-match)
                      (< (abs (- p current-match)) closest-diff))
              (setq closest-diff (abs (- p current-match))
                    closest-match current-match)))
          (goto-char (save-excursion
                       (goto-char closest-match)
                       (let ((offset nil))
                         (cond ((and (>= (length re-left) (length re-right))
                                     (setq offset (string-match re-left (buffer-substring (point-at-bol) (point-at-eol)))))
                                (+ closest-match offset (length line-left)))
                               ((and (< (length re-left) (length re-right))
                                     (setq offset (string-match re-right (buffer-substring (point-at-bol) (point-at-eol)))))
                                (+ closest-match offset))
                               (t
                                (if (> (+ closest-match (length line-left)) (point-at-eol))
                                    (point-at-eol)
                                  (+ closest-match (length line-left))))
                               ))))
          )))))

(defun go-back-make-location ()
  (save-excursion
    (when (go-back-ignore-line-p)
      (go-back-previous-line)
      (goto-char (point-at-eol)))
    (list (current-buffer)
          (point)
          (window-start)
          (buffer-substring-no-properties (point-at-bol) (point))
          (buffer-substring-no-properties (point) (point-at-eol))
          (save-excursion
            (go-back-previous-line)
            (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          (save-excursion
            (go-back-next-line)
            (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          )))

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
  (push-mark)
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
  ;;(go-back-dump-state)
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
  ;;(go-back-dump-state)
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
                                        ;;(highlight-phrase)
                                        ;;(highlight-symbol-at-point)
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
                                         eval-region
                                         eval-region-or-defun
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
                (eq tc 'go-back-push)
                (eq tc 'go-back-pre-command-trigger))
      ;; (loop for xs in go-back-cursor-commands
      ;;       until (when (some 'identity (mapcar (apply-partially 'eq tc) xs))
      ;;               (setq go-back-current (go-back-make-location))))
      (loop for ys in go-back-trigger-command-symbols
            until (when (some 'identity (mapcar (apply-partially 'eq tc) ys))
                    (setq triggered `(command ,ys))))
      (if triggered
          (unless (some 'identity (mapcar (apply-partially 'eq lc) (second triggered)))
            (unless (or (eq lc 'go-back-next)
                        (eq lc 'go-back-prev)
                        (eq lc 'go-back-push))
              (when (buffer-file-name (current-buffer))
                (go-back-push)
                )))
        ;; (save-excursion
        ;;   (save-restriction
        ;;     (unless (go-back-ignore-line-p)
        ;;       (print (go-back-make-location)))))
        ))))

(add-hook 'pre-command-hook 'go-back-pre-command-trigger)

;;(remove-hook 'pre-command-hook 'go-back-pre-command-trigger)

(provide 'go-back)
