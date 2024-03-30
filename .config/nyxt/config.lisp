(defvar *my-keymap* (make-keymap "my-map"))

(define-key *my-keymap*
            "C-d" 'execute-command "C-b" 'nothing
            "C-u" 'execute-command "C-f" 'nothing)

; TODO can't get my-mode working
;(define-mode my-mode
;    nil
;  "Dummy mode for the custom key bindings in *my-keymap*."
;  ((keyscheme-map
;    (nkeymaps/core:make-keyscheme-map keyscheme:vi-normal *my-keymap*))))

;(define-configuration web-buffer "Enable this mode by default." ((default-modes (pushnew 'my-mode %slot-value%))))
