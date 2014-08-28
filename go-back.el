;; go-back.el -- just brings you back to where you were before
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
;; matt harrison <matthewharrison@gmail.com>
;; dmitry gutov <dgutov@yandex.ru>

;; what I want
;; 1. remember the last position where I made something meaningful [eval-region, save-buffer, ...]
;; 2. restore that position while remembering the position I am currently at
;; 3. scroll through remembered positions until I find the one I want to do something at,
;;    then that one should be the one I go back to first after the next time I restored a
;;    position
;;
;; lets try that again:
;; I want to remember TWO positions, the one I am working at RIGHT NOW, and the one I where
;; I was working at BEFORE
;; the only way to implement that is to assume that when I restore a position, that I want to
;; work there and therefore remember that position IN ADDITION to the position where I was
;; before restoring
;;
;; do4: 1 2 3 4
;;            *
;; f11: 1 2 3 4
;;          *<
;; f11: 1 2 3 4
;;        *<
;; do :       4 3 2 1
;;                *
;; do :       4 3 2 5 1
;;                 +*
;; do :       4 3 2 5 6 1
;;                   +*

(require 'cl)

(defvar go-back-past '())
(make-variable-buffer-local 'go-back-past)

(defvar go-back-future '())
(make-variable-buffer-local 'go-back-future)

(defvar go-back-current nil)
(make-variable-buffer-local 'go-back-current)

(defvar go-back-before '(nil (point-min) (point-min)))
(make-variable-buffer-local 'go-back-before)

(defun go-back-reset ()
  (interactive)
  (setq go-back-past '()
        go-back-future '()
        go-back-current nil
        go-back-before nil))

(defvar go-back-history '())
(make-variable-buffer-local 'go-back-history)

(defvar go-back-last-jump-cost 0)
(make-variable-buffer-local 'go-back-last-jump-cost)

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
                           (cond ((eq (length words) 1)
                                  (add-to-list 'res (regexp-quote (pop words)) t))
                                 ((null res)
                                  (add-to-list 'res (regexp-quote (concat (pop words) " ")) t))
                                 ((null (cdr words))
                                  (add-to-list 'res (regexp-quote (concat " " (pop words))) t))
                                 (t
                                  (add-to-list 'res (regexp-quote (concat " " (pop words) " ")) t))))
                         res))))
    (let* ((buffer (nth 0 loc))
           (p (eval (nth 1 loc)))
           (line-left (nth 3 loc))
           (line-right (nth 4 loc))
           (re-left (when line-left (regexp-quote line-left)))
           (re-right (when line-right (regexp-quote line-right)))
           (line-prev (nth 5 loc))
           (line-next (nth 6 loc))
           (words-1 (line-to-re (concat line-left line-right)))
           (words-2 (line-to-re line-prev))
           (words-3 (line-to-re line-next))
           (final-point nil))
      (when loc
        (switch-to-buffer (or buffer (other-buffer)))
        ;; goto original point and try to match line to either left or right regex
        (unless final-point
          (setq final-point
                (save-excursion
                  (goto-char p)
                  (cond ((re-search-forward re-right (point-at-eol) t)
                         (re-search-backward re-right (point-at-bol) t)
                         (point))
                        ((re-search-backward re-left (point-at-bol) t)
                         (re-search-forward re-left (point-at-eol) t)
                         (point)))))))
      ;; find every line with a matching word, normalize, use line with most matches
      (unless final-point
        (setq final-point
              (save-excursion
                (let ((matches-1 (save-excursion
                                   (remove-duplicates
                                    (loop for w1 in words-1
                                          if (string-equal w1 "#bobp#")
                                          collect '(0)
                                          else
                                          if (string-equal w1 "#eobp#")
                                          collect `(,(point-max))
                                          else
                                          if (string-match ".*[a-zA-Z0-9]+.*" w1)
                                          do (goto-char (point-min))
                                          and
                                          collect (loop while (re-search-forward w1 nil t)
                                                        collect (point-at-bol))))))
                      (matches-2 (save-excursion
                                   (remove-duplicates
                                    (loop for w2 in words-2
                                          if (string-equal w2 "#bobp#")
                                          collect '(0)
                                          else
                                          if (string-equal w2 "#eobp#")
                                          collect `(,(point-max))
                                          else
                                          if (string-match ".*[a-zA-Z0-9]+.*" w2)
                                          do (goto-char (point-min))
                                          and
                                          collect (loop while (re-search-forward w2 nil t)
                                                        collect (save-excursion (go-back-previous-line)
                                                                                (point-at-bol)))))))
                      (matches-3 (save-excursion
                                   (remove-duplicates
                                    (loop for w3 in words-3
                                          if (string-equal w3 "#bobp#")
                                          collect '(0)
                                          else
                                          if (string-equal w3 "#eobp#")
                                          collect `(,(point-max))
                                          else
                                          if (string-match ".*[a-zA-Z0-9]+.*" w3)
                                          do (goto-char (point-min))
                                          and
                                          collect (loop while (re-search-forward w3 nil t)
                                                        collect (save-excursion (go-back-next-line)
                                                                                (point-at-bol))))))))
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
                         (closest-match nil))
                    (when all-matches
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
                      (goto-char closest-match)
                      (let* ((offset nil)
                             (from (save-excursion (dotimes (n 2) (go-back-previous-line))
                                                   (point-at-bol)))
                             (to (save-excursion (dotimes (n 2) (go-back-next-line))
                                                 (point-at-eol)))
                             (new-p (cond ((and (>= (length re-left) (length re-right))
                                                (setq offset (save-excursion (goto-char from)
                                                                             (when (re-search-forward re-left to t)
                                                                               (- (point) closest-match)))))
                                           (+ closest-match offset))
                                          ((and (< (length re-left) (length re-right))
                                                (setq offset (save-excursion (goto-char from)
                                                                             (when (re-search-forward re-right to t)
                                                                               (- (point) closest-match)))))
                                           (- (+ closest-match offset) (length line-right)))
                                          (t
                                           (+ closest-match (length line-left))))))
                        (goto-char new-p)))))
                (point))))
      ;; search for complete line in buffer
      (unless final-point
        (setq final-point
              (save-excursion
                (goto-char (point-min))
                (cond ((and (> (length (concat re-left re-right)) 1)
                            (re-search-forward (concat re-left re-right) nil t))
                       (- (point) (length line-right)))
                      ((and (> (length re-left) 1)
                            (re-search-forward re-left nil t))
                       (point))
                      ((and (> (length re-right) 1)
                            (re-search-forward re-right nil t))
                       (- (point) (length line-right)))))))
      ;; move to final point
      (goto-char (or final-point p)))))

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
                     ","
                     (prin1-to-string (second go-back-before))
                     ",["
                     (mapconcat #'prin1-to-string (mapcar #'second r) ",")
                     "] cost: " (prin1-to-string go-back-last-jump-cost)))))

(defun* go-back-shift (&optional (direction :left))
  (interactive)
  (case direction
    (:right
     (when (car (last go-back-past))
       (add-to-list 'go-back-future (car (last go-back-past)))
       (add-to-list 'go-back-history `(shift ,direction))
       (setq go-back-past (butlast go-back-past))
       (when (null go-back-past)
         (setq go-back-past (last go-back-future)
               go-back-future (butlast go-back-future)))
       (setq go-back-current (car (last go-back-past)))))
    (:left
     (when (car go-back-future)
       (add-to-list 'go-back-past (car go-back-future) t)
       (add-to-list 'go-back-history `(shift ,direction))
       (setq go-back-future (cdr go-back-future))
       (when (null go-back-future)
         (setq go-back-future (list (first go-back-past))
               go-back-past (cdr go-back-past))))
     (setq go-back-current (car go-back-future)))
    ))

(defun go-back-loc-equal (a b)
  (or (and (eq (nth 0 a) (nth 0 b))
           (string-equal (nth 3 a) (nth 3 b))
           (string-equal (nth 4 a) (nth 4 b)))
      (and (equal (nth 0 a) (nth 0 b))
           (= (nth 1 a) (nth 1 b)))))

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
     (setq go-back-current loc))))

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
       (setq go-back-past (butlast go-back-past))))))

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
       (let ((invoke-location (go-back-make-location)))
         (go-back-go go-back-current)
         (setq go-back-before invoke-location)
         (when (= (point) (nth 1 invoke-location))
           (go-back-shift :right)
           (setq go-back-current (car (last go-back-past)))
           (go-back-go go-back-current)
           (setq go-back-before invoke-location))
         (when (eq go-back-current (car go-back-future))
           (go-back-shift :left))
         (go-back-push loc :right)
         (setq go-back-current (car (last go-back-past))))
       ))))

;; blah

;; foo

;; bar

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
       (if go-back-before
           (progn
             (go-back-go go-back-before)
             (setq go-back-before nil))
         (setq go-back-current (car go-back-future))
         (go-back-go go-back-current))
       ))))

(setq go-back-trigger-command-symbols '((isearch-mode
                                         isearch-forward
                                         isearch-repeat-forward
                                         isearch-backward
                                         isearch-repeat-backward
                                         isearch-resume
                                         isearch-printing-char
                                         isearch-abort
                                         isearch-exit
                                         isearch-other-control-char
                                         query-replace-regexp)
                                        (undo-tree-undo
                                         undo-tree-redo)
                                        (highlight-symbol-prev
                                         highlight-symbol-jump
                                         highlight-symbol-next
                                         highlight-symbol-jump
                                         smartscan-symbol-go-forward
                                         smartscan-symbol-go-backward
                                         smartscan-symbol-replace)
                                        (sourcemarker-visit)
                                        (ace-jump-mode)
                                        (imenu
                                         imenu-many)
                                        (etags-select-find-tag
                                         find-tag
                                         pop-tag-mark
                                         tags-search
                                         tags-loop-continue)
                                        (mwheel-scroll
                                         cua-scroll-up
                                         cua-scroll-down
                                         scroll-up
                                         scroll-up-mark
                                         scroll-up-nomark
                                         scroll-up-command
                                         scroll-down
                                         scroll-down-mark
                                         scroll-down-nomark
                                         scroll-down-command
                                         beginning-of-buffer
                                         end-of-buffer
                                         ;;next-line
                                         ;;previous-line
                                         )
                                        (save-buffer
                                         switch-to-buffer
                                         save-window-excursion-buffer)
                                        (eval-defun
                                         eval-last-sexp
                                         eval-buffer
                                         eval-region
                                         eval-region-or-defun
                                         compile)
                                        (project-jump-definition
                                         project-jump-regexp)))

;; (setq go-back-cursor-commands '((next-line
;;                                  next-line-mark
;;                                  next-line-nomark
;;                                  previous-line
;;                                  previous-line-mark
;;                                  previous-line-nomark
;;                                  backward-char-mark
;;                                  backward-char-nomark
;;                                  backward-word
;;                                  backward-word-mark
;;                                  backward-word-nomark
;;                                  backward-sexp
;;                                  backward-sexp-mark
;;                                  backward-sexp-nomark
;;                                  backward-sentence
;;                                  backward-paragraph
;;                                  backward-paragraph-mark
;;                                  backward-paragraph-nomark
;;                                  forward-char-mark
;;                                  forward-char-nomark
;;                                  forward-word
;;                                  forward-word-mark
;;                                  forward-word-nomark
;;                                  forward-sexp
;;                                  forward-sexp-mark
;;                                  forward-sexp-nomark
;;                                  forward-sentence
;;                                  forward-paragraph
;;                                  forward-paragraph-mark
;;                                  forward-paragraph-nomark
;;                                  end-of-line
;;                                  end-of-line-mark
;;                                  end-of-line-nomark
;;                                  beginning-of-line
;;                                  beginning-of-line-mark
;;                                  beginning-of-line-nomark)))

(defun go-back-pre-command-trigger ()
  (let* ((tc this-command)
         (lc last-command)
         (triggered nil))
    (unless (or (eq tc 'go-back-next)
                (eq tc 'go-back-prev)
                (eq tc 'go-back-push)
                (eq tc 'go-back-pre-command-trigger))
      (loop for ys in go-back-trigger-command-symbols
            until (when (some 'identity (mapcar (apply-partially 'eq tc) ys))
                    (setq triggered `(command ,ys))))
      (if triggered
          (progn
            (unless (some 'identity (mapcar (apply-partially 'eq lc) (second triggered)))
              (unless (or (eq lc 'go-back-next)
                          (eq lc 'go-back-prev)
                          (eq lc 'go-back-push))
                (when (buffer-file-name (current-buffer))
                  (go-back-push)))))
        (dolist (ov (overlays-at (point)))
          (unless (overlay-get ov 'go-back-to)
            (when (eq (overlay-get ov 'jump-highlight) 'view)
              (overlay-put ov 'go-back-to t)
              (go-back-push))))))))

(eval-after-load "go-back"
  '(progn
     ;;(kill-local-variable 'pre-command-hook)
     (add-hook 'pre-command-hook 'go-back-pre-command-trigger t nil)))

(provide 'go-back)
