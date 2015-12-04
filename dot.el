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

(defun dot-append-command (command)
  (if dot-cut
      (setq dot-cut nil dot-last-commands (list command))
    (setq dot-last-commands (append dot-last-commands (list command)))))

(defun dot-append-insert-command ()

(defun dot-pre-command-hook ()
  (setq dot-last-position (point)
        dot-last-buffer-size (save-restriction (widen) (point-max))))

(defun dot-post-command-hook ()
  (cond ((or (member real-this-command dot-standard-insertion-commands)
             (member real-this-command dot-additional-insertion-commands))
         (dot-append-command (buffer-substring-no-properties (point) dot-last-position)))
        ((or (member real-this-command dot-standard-deletion-commands)
             (member real-this-command dot-additional-deletion-commands))
         (dot-append-command real-this-command))
        (t (setq dot-cut t))))

(defun dot-mode (&optional arg)
  (if (or (not arg)
          (and (numberp arg) (> arg 0)))
      (progn
        (add-hook 'post-command-hook 'dot-post-command-hook)
        (add-hook 'pre-command-hook 'dot-pre-command-hook))
    (remove-hook 'post-command-hook 'dot-post-command-hook)
    (remove-hook 'pre-command-hook 'dot-pre-command-hook)))

(provide 'dot)
