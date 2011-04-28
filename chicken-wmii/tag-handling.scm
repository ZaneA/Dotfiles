;;
;; Tag Handling
;;

(add-event-handler create-tag     (lambda (event tag) (wmii:write-tab "lbar" tag tag wmii-normcolors)))
(add-event-handler destroy-tag    (lambda (event tag) (wmii:destroy-tab "lbar" tag)))
(add-event-handler focus-tag      (lambda (event tag)
                                    (if (member tag (wmii:tabs "lbar"))
                                        (wmii:write-tab "lbar" tag tag wmii-focuscolors))))
(add-event-handler unfocus-tag    (lambda (event tag)
                                    (if (member tag (wmii:tabs "lbar"))
                                        (wmii:write-tab "lbar" tag tag wmii-normcolors))))
(add-event-handler urgent-tag     (lambda (event tag client?) (wmii:write-tab "lbar" tag (string-append "*" tag))))
(add-event-handler not-urgent-tag (lambda (event tag client?) (wmii:write-tab "lbar" tag tag)))
(add-event-handler left-bar-click (lambda (event button tab) (wmii:goto-tag tab)))

(add-key '("t")                   (and-let* ((tag (wimenu (wmii:tags)))) (wmii:goto-tag tag)))
(add-key '("Shift" "t")           (and-let* ((tag (wimenu (wmii:tags)))) (wmii:client-tags-set! (list tag))))

; Moving / sending windows
(for-each (lambda (x) (add-key `(,(cdr x))              (wmii:navigate-to (->string (car x))))) directions)
(for-each (lambda (x) (add-key `("Shift" ,(cdr x))      (wmii:send-to (->string (car x))))) directions)

; Moving / sending to tags
(for-each (lambda (x) (add-key `(,(->string x))         (wmii:goto-tag x))) (iota 10))
(for-each (lambda (x) (add-key `("Shift" ,(->string x)) (wmii:client-tags-set! (list (->string x))))) (iota 10))

(let ((curtag (wmii:tag))) 
  (for-each (cut wmii:destroy-tab "lbar" <>) (wmii:tabs "lbar"))
  (for-each (lambda (t)
              (if (string=? curtag t)
                  (wmii:write-tab "lbar" t t wmii-focuscolors)
                  (wmii:write-tab "lbar" t t wmii-normcolors)))
            (wmii:tags)))
