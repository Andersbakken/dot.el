# dot.el
Naive minor mode to provide similar functionality to vim's dot command

To use do the following:

(require 'dot)
(enable-dot 1)
(define-key 'global-map (kbd "C-.") 'dot)