;;; pocketcompletion.el --- tree style completion intended for tablets

;;; Commentary:
;; 
;; pocketcompletion aims to provide a style of completion that is
;; useful on a small tablet device, like the Openmoko Freerunner.

;; TODO currently this seems to be broken due to API changes in
;; dict-tree.el(maybe). 

;;; History:
;; 

;; see http://www.emacswiki.org/emacs/Tries, for some newer code
;;git clone http://www.dr-qubit.org/git/predictive.git predictive
(require 'dict-tree)
(require 'trie)
(require 'cl)

;;; Code:
(setq my-tree (dict-create-type 'dict-tree-test "dictionary"));;TODO doesnt work anymore


;;this inserts all interactively callable commands in a dict(about 9000, takes some time!)
 (mapatoms (lambda (z) (if (commandp z t)
                           (dict-insert my-tree (symbol-name z) '(x)))))

;; its possible to populate the dict from a file:
;;(dict-populate-from-file my-tree "/home/joakim/ternarytrees/tstdata.txt")

;;(dict-insert my-tree "let-there-be-sunshine")
;;(dict-insert my-tree "let-it-rain")
;;(progn (setq x 0) (mapatoms (lambda (z) (if (commandp z)  (setq x (1+ x))))) x);;count commands
;;(dict-dump-words-to-buffer my-tree);;dump words to a buffer

;;(tstree-map (lambda (x y) (message "a:%s d:%s" x y)) (dic-tstree my-tree) 1)
;;(tst-node-split (tst-node-equal (tst-tree-root (dic-tstree my-tree)) ))


;; (define-minor-mode pocketcompletion-mode
;;        "Toggle pocketcompletion mode"
;;       ;; The initial value.
;;       :init-value nil
;;       ;; The indicator for the mode line.
;;       :lighter " pocketcompletion"
;;       ;; The minor mode bindings.
;;       :group 'pocketcompletion
;;       :global t
;;       :keymap
;;       '(([tool-bar pocketcompletion] .
;;          (menu-item "pocketcompletion" pocketcompletion
;;                   :image (image :type xpm :file "zoom-in.xpm"))))
;;       (message "pocketcompletion minor body %s" pocketcompletion-mode)
;;       ;(pocketcompletion-enable-toolbar-button)

;;       )


;; (defun pocketcompletion-enable-toolbar-button ()
;;   (define-key global-map [tool-bar pocketcompletion]
;;    '(menu-item "pocketcompletion" pocketcompletion
;;                :image (image :type xpm :file "zoom-in.xpm")))
;;   )


;; this doesnt work too well
;; the tool bar button doesn seem to show consistenly
;; i just took a random icon
;; (define-key global-map [tool-bar pocketcompletion]
;;   '(menu-item "pocketcompletion" pocketcompletion
;;               :image (image :type xpm :file "zoom-in.xpm")))

;(tool-bar-add-item "zoom-in" 'pocketcompletion 'pocketcompletion)

;;         from Stefan Monnier:
;; The problem is that whenever you lookup [tool-bar] in global-map (and
;; such a lookup takes place to find the map into which to add the
;; pocketcompletion element), you receive a new keymap, built fresh by
;; tool-bar-make-keymap.

;; So you want to manipulate tool-bar-map directly.
;; Note that tool-bar-map is buffer-local, so you won't be able to add
;; elements truly globally.  For that you'll need to advise
;; tool-bar-make-keymap :-(

;;ok, so do an experimental redefinition:

(defvar global-tool-bar-map)

(setq global-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (dolist (x '((pocketcompletion . "zoom-in"))
               map)
      (tool-bar-local-item
       (cdr x) (car x) (car x)  map))))


(defun tool-bar-make-keymap-1 ()
  "Generate an actual keymap from `tool-bar-map', without caching."
  (mapcar (lambda (bind)
            (let (image-exp plist)
              (when (and (eq (car-safe (cdr-safe bind)) 'menu-item)
			 ;; For the format of menu-items, see node
			 ;; `Extended Menu Items' in the Elisp manual.
			 (setq plist (nthcdr (if (consp (nth 4 bind)) 5 4)
					     bind))
			 (setq image-exp (plist-get plist :image))
			 (consp image-exp)
			 (not (eq (car image-exp) 'image))
			 (fboundp (car image-exp)))
		(if (not (display-images-p))
		    (setq bind nil)
		  (let ((image (eval image-exp)))
		    (unless (and image (image-mask-p image))
		      (setq image (append image '(:mask heuristic))))
		    (setq bind (copy-sequence bind)
			  plist (nthcdr (if (consp (nth 4 bind)) 5 4)
					bind))
		    (plist-put plist :image image))))
	      bind))
	  (cons 'keymap (append (cdr global-tool-bar-map)
                  (cdr tool-bar-map)))
          ))




(defun pocketcompletion-choice-stack-as-string ()
  "Return the current stack of choices as a string."
  (mapconcat  (lambda (x) x) (reverse pocketcompletion-choice-stack) ""))

(defun pocketcompletion-back (e)
  "Called when the header line button 'back' is pressed(interactive E).
This will pop the choice stack."
  (interactive "e")
  (message "header back %s" e)
  (pop pocketcompletion-choice-stack)
  (pocketcompletion-mkbuffer (pocketcompletion-choice-stack-as-string)))

(defun pocketcompletion-do (e)
  "Called when the header line button 'do' is pressed(E).
This will execute the current command in the choice stack."
  (interactive "e")
  (message "header do %s" pocketcompletion-choice-stack)
  ;;(funcall (symbol-function (intern-soft (pocketcompletion-choice-stack-as-string))))
  (eval (car (read-from-string (concat "("(pocketcompletion-choice-stack-as-string) ")")))))

(defun pocketcompletion-mk-header-keymap (fn)
  "Make a keymap for a header button.
FN will be called when the button is pressed."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] fn)
    map))


(defvar pocketcompletion-choice-stack
  nil
  "Stack of choices so we can go back in choices.")

(defun pocketcompletion-mkheader (text)
  "Make the header line, which displays the buttons and the current choice stack, as TEXT."
  (setq header-line-format
        (list (propertize "[back]"
                          'local-map  (pocketcompletion-mk-header-keymap 'pocketcompletion-back)
                          'mouse-face 'highlight)
              " "
              (pocketcompletion-choice-stack-as-string)
              " "
              (propertize "[do]"
                          'local-map  (pocketcompletion-mk-header-keymap 'pocketcompletion-do)
                          'mouse-face 'highlight)
              )))

(defun pocketcompletion()
  (interactive)
  (setq pocketcompletion-choice-stack nil)
  (pocketcompletion-mkbuffer ""))



(defun pocketcompletion-erase()
  (erase-buffer))

(defun pocketcompletion-mkbuffer (text)
  "Make the pocketcompletion buffer, TEXT is the choicestack."
(let* ((temp-buffer (get-buffer-create "*Pocketcompletion*")))
    ;(switch-to-buffer-other-window temp-buffer)
    (pop-to-buffer temp-buffer)
    (with-current-buffer temp-buffer

      (pocketcompletion-erase)
      (pocketcompletion-mkheader text)
      (pocketcompletion-mkbuttons text)
      (princ "\n"  temp-buffer)
      (if (functionp (intern-soft text)) (princ  (documentation (intern-soft text))  temp-buffer))
    )))

;;(tstree-mymap (dic-tstree my-tree))
(defun pocketcompletion-button-callback (overlay)
  "A callback for the choice buttons, will be called with OVERLAY for the button."
  (let ((text (overlay-get  overlay 'text))
        (sub-text (overlay-get  overlay 'sub-text)))
    (message text)
    (push sub-text pocketcompletion-choice-stack)

    (pocketcompletion-mkbuffer text)

    ))


(defun pocketcompletion-mkbuttons (prefix)
  "Make choice buttons.  PREFIX is the text so far in the choice stack."
  (mapcar (lambda (button-label)
            (insert-button button-label
                           'action 'pocketcompletion-button-callback
                           'follow-link t
                           'text (concat prefix button-label)
                           'sub-text button-label
                           )
            (insert " "))
          (pocketcompletion-tst prefix)))

(defun pocketcompletion-tst (str)
  "Dont remember this STR."
  (pocketcompletion-mymap (tst-node-find  (dic-tstree my-tree) str)))

(defun pocketcompletion-mymap (tree)
  "Only other doing something if TREE is not empty."
  (let (stack str node  flags result
              (buffer (current-buffer))
              )
    
    ;; initialise the stack (tree/str pairs)
    (push tree stack)
    (push "" stack)
    
    ;; Keep going until we've traversed all nodes (node stack is empty)
    (while (not (null stack))
      ;;        (princ "------------------------\n" buffer)
      (setq str (pop stack))
      (setq node (pop stack))

      (setq flags (list (if (tst-node-high node) 1 0)
                        (if (tst-node-equal node) 1 0)
                        (if (tst-node-low node) 1 0)))
;;;         (unless (equal '(0 1 0) flags)
;;;           (princ (format "split:%s hql:%s   str:%s \n"
;;;                          (if (tst-node-split node) (string (tst-node-split node)) nil)
;;;                          flags
;;;                          str)
;;;                  buffer))
      ;; add the high child to the stack, if it exists
      (when (tst-node-high node)
        (push (tst-node-high node) stack)
        (push str stack))
      ;; If we're at a data node call FUNCTION, otherwise add the equal
      ;; child to the stack.
      (if (null (tst-node-split node))
          t
        ;;(princ (format "-- data node: %s %s\n" str (tst-node-equal node)) buffer)
        ;;(push (tst-node-equal node) stack)
        (push [nil nil nil nil ] stack)
        (setq result (append result (list
                                     (pocketcompletion-follow-equal-until-branch
                                      (tst-node-equal node)
                                      (string (tst-node-split node))
                                      buffer))))
        (push (concat str (string (tst-node-split node)))  stack)
        )
      ;; add the low child to the stack, if it exists
      (when (tst-node-low node)
        (push (tst-node-low node) stack)
        (push str stack))
      )
    (sort result (lambda (x y) (string< x y)))
    ))

(defun pocketcompletion-follow-equal-until-branch (tree bl buffer)
  "Follow the equal branch of a ternary TREE, until it branches.
Argument BL .
Argument BUFFER ."
  (let (result
        (node  tree ))
    (progn
      (setq result bl)
      (while (not (or (null node)
                      (null (tst-node-split node))
                      (tst-node-high node)
                      (tst-node-low node))
                  )
        (setq result  (concat result (string (tst-node-split node))))
        (setq node (tst-node-equal node))))result))
(provide 'pocketcompletion)

;;; pocketcompletion.el ends here
