;;; -*- Gerbil -*-
(import :std/sugar
        :std/format
        :lho/shsx/lib
        ./lib)
(export #t)

;; Base template with common structure
(define (base-template title content)
  (shsx
   (html:
    (head:
     (title: ,title)
     (meta: charset: "utf-8")
     (script: src: "https://unpkg.com/htmx.org@1.9.10")
     (link: rel: "stylesheet" href: "https://cdn.jsdelivr.net/gh/luciusmagn/berkeley-css/dist/berkeley.css")
     (link: rel: "stylesheet" href: "/static/style.css"))
    (body: ,content))))

;; Index page
(define (index-page)
  (base-template
   "QONS"
   (shsx
    (main:
     (section: class: "container"
               (h1: class: "qons-title"
                    "Q0NS")
               (form:
                class: "qons-code-form"
                (div: class: "qons-input-container"
                      (input: type: "text"
                              name: "roomId"
                              class: "room-code-input"
                              placeholder: "room code"
                              pattern: "[0-9]+"
                              required: ""))
                (div: class: "qons-buttons"
                      (button: class: "primary"
                               onclick: "window.location.href='/r/' + document.querySelector('[name=roomId]').value"
                               type: "button"
                               "Join")
                      (button: hx-post: "/r"
                               hx-swap: "none"
                               type: "button"
                               class: "secondary"
                               "Create Room"))))))))

;; Room page
(define (room-page room)
  (base-template
   (format "Room #~a" (room-id room))
   (shsx
    (main:
     (h1: "Room #" (span: id: "room-id" ,(number->string (room-id room))))
     (form: hx-post: ,(format "/r/~a/questions" (number->string (room-id room)))
            hx-swap: "none"
            hx-target: "#questions"
            (textarea: name: "text" required: "")
            (input: type: "text"
                    name: "author"
                    placeholder: "Name (optional)")
            (button: "Ask"))))))
;;,(questions-list room '())))))

;; Questions list partial (for HTMX updates)
(define (questions-list room questions-with-votes)
  (shsx
   (div: id: "questions"
         hx-get: ,(format "/r/~a/questions" (room-id room))
         hx-trigger: "every 2s, questionAdded from:body"
         hx-swap: "outerHTML swap:*"
         class: "questions"
         ,@(map (lambda (qvi)
                  (question-item (room-id room)
                                 (car qvi)     ; question
                                 (cadr qvi)    ; votes
                                 (caddr qvi))) ; is-admin?
                questions-with-votes))))

;; Single question item
(define (question-item room-id q votes is-admin?)
  (shsx
   (div: class: "question"
         id: ,(format "q-~a" (question-id q))
         (p: ,(question-text q))
         ,(@when (question-author q)
            (small: ,(question-author q)))
         (button: hx-post: ,(format "/r/~a/questions/~a/up"
                                    room-id
                                    (question-id q))
                  hx-swap: "none"
                  class: "upvote"
                  "▲ " ,(number->string votes))
         ,(@when is-admin?
            (button: hx-delete: ,(format "/r/~a/questions/~a"
                                         room-id
                                         (question-id q))
                     hx-swap: "none"
                     hx-confirm: "Delete this question?"
                     class: "delete"
                     "×")))))
