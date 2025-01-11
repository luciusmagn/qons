;;; -*- Gerbil -*-
(import :std/sugar
        :std/format
        :lho/shsx/lib
        ./lib)
(export #t)

;; Base template with common structure
(def (base-template title content)
  (shsx
   (html:
    (head:
     (title: ,title)
     (meta: charset: "utf-8")
     (script: src: "https://unpkg.com/htmx.org@1.9.10")
     (link: rel: "stylesheet" href: "/static/style.css"))
    (body: ,content))))

;; Index page
(def (index-page)
  (base-template
   "QONS - Question Room"
   (shsx
    (main:
     (h1: "Question Room")
     (form: hx-post: "/r" hx-swap: "none"
            (button: "Create New Room"))
     (form: hx-get: "/r/${roomId}"
            hx-trigger: "submit"
            hx-target: "body"
            hx-push-url: "true"
            (input: type: "text"
                    name: "roomId"
                    placeholder: "Enter room code"
                    pattern: "[0-9]+"
                    required: "")
            (button: "Join Room"))))))

;; Room page
(def (room-page room)
  (base-template
   (format "Room #~a" (room-id room))
   (shsx
    (main:
     (h1: "Room #" (span: id: "room-id" ,(room-id room)))
     (form: hx-post: ,(format "/r/~a/questions" (room-id room))
            hx-swap: "none"
            hx-target: "#questions"
            (textarea: name: "text" required: "")
            (input: type: "text"
                    name: "author"
                    placeholder: "Name (optional)")
            (button: "Ask"))
     ,(questions-list room '())))))

;; Questions list partial (for HTMX updates)
(def (questions-list room questions-with-votes)
  (shsx
   (div: id: "questions"
         hx-get: ,(format "/r/~a/questions" (room-id room))
         hx-trigger: "every 2s, questionAdded from:body"
         hx-swap: "outerHTML swap:*"
         class: "questions"
         ,(@begin
            ,@(map (match <>
                     ((list q votes is-admin?)
                      (question-item (room-id room) q votes is-admin?)))
                   questions-with-votes)))))

;; Single question item
(def (question-item room-id q votes is-admin?)
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
