;; Naive vim-like dot implementation
;; To use:
;; (require 'dot)
;; (enable-dot 1)
;; (define-key 'global-map (kbd "C-.") 'dot)

(defvar dot-last-commands nil)
(defvar dot-last-position nil)
(defvar dot-cut nil)
(defun dot ()
  (interactive)
  (setq dot-cut t)
  (let ((cur dot-last-commands))
    (while cur
      (cond ((functionp (car cur)) (call-interactively (car cur)))
            ((stringp (car cur)) (insert (car cur)))
            (t))
      (setq cur (cdr cur)))))

(defun dot-clear ()
  (interactive)
  (setq dot-last-commands nil))

(defconst dot-standard-insertion-commands (list 'self-insert-command 'yank))
(defconst dot-standard-deletion-commands (list 'delete-forward-char
                                               'delete-backward-char
                                               'kill-region
                                               'kill-word
                                               'backward-kill-word
                                               'backward-delete-char-untabify))

(defvar dot-additional-insertion-commands nil)
(defvar dot-additional-deletion-commands nil)

(defun dot-pre-command-hook ()
  (setq dot-last-position (point)
        dot-last-buffer-size (save-restriction (widen) (point-max))))

(defun dot-post-command-hook ()
  (let ((command (cond ((or (member real-this-command dot-standard-insertion-commands)
                            (member real-this-command dot-additional-insertion-commands))
                        (and (not (= (point) dot-last-position))
                             (buffer-substring-no-properties (point) dot-last-position)))
                       ((or (member real-this-command dot-standard-deletion-commands)
                            (member real-this-command dot-additional-deletion-commands))
                        real-this-command)
                       (t (setq dot-cut t) nil))))
    (when command
      (if dot-cut
          (setq dot-cut nil dot-last-commands (list command))
        (setq dot-last-commands (append dot-last-commands (list command)))))))

(defun enable-dot (&optional arg)
  (if (and (numberp arg)
           (<= arg 0))
      (progn
        (remove-hook 'post-command-hook 'dot-post-command-hook)
        (remove-hook 'pre-command-hook 'dot-pre-command-hook))
    (add-hook 'post-command-hook 'dot-post-command-hook)
    (add-hook 'pre-command-hook 'dot-pre-command-hook)))

(provide 'dot)
