;;; -*- Gerbil -*-
(import :std/sugar
        :std/format
        :lho/shsx/lib
        :lho/fxns/lib
        ./lib)
(export #t)

;; TODO:
;;   - logging in via braiins email token (???)

;; Base template with common structure
(fn :ret base-template ((title : string?) (content : any?) -> any?)
    (shsx
     (html:
      (head:
       (title: ,title)
       (meta: charset: "utf-8")
       (script: src: "https://unpkg.com/htmx.org@1.9.10")
       (link: rel: "stylesheet" href: "https://cdn.jsdelivr.net/gh/luciusmagn/berkeley-css/dist/berkeley.css")
       (link: rel: "stylesheet" href: "/static/style.css"))
      (body:
       ,content
       (script: "// This part of code is copied from https://stackoverflow.com/a/73593579
               document.body.addEventListener('htmx:beforeOnLoad', function(evt) {
                   if (evt.detail.xhr.status >= 300 && evt.detail.xhr.status < 500) {
                       evt.detail.shouldSwap = true;
                       evt.detail.idError = false;
                   }
               });

               function autoResizeTextarea(textarea) {
                 textarea.style.height = 'auto';
                 textarea.style.height = `${textarea.scrollHeight}px`;
               }

               document.addEventListener('DOMContentLoaded', () => {
                 const textarea = document.querySelector('#question-input');
                 if (textarea !== null) {
                      autoResizeTextarea(textarea);
                      textarea.addEventListener('input', () => autoResizeTextarea(textarea));
                 }
               });")))))

;; Index page
(fn :ret index-page ((admin-rooms : (list-of (pair-of number? room?))) (recent-rooms : (list-of room?)) -> any?)
    (base-template
     "QONS"
     (shsx
      (main: class: "invis-container"
             (a: href: "/"
                 class: "qons-title"
                 "'(Q0NS? ...)")
             (section: class: "container main-qons"
                       (div: style: "display: flex; flex-direction: row"
                             (div: style: "display: flex; flex-direction: column"
                                   (section: class: "container reset-inline-margin"
                                             (form:
                                              class: "qons-code-form"
                                              (div: class: "qons-input-container"
                                                    (input: type: "text"
                                                            name: "roomId"
                                                            class: "room-code-input"
                                                            placeholder: "R00M C0DE"
                                                            pattern: "[0-9]+"
                                                            required: ""))
                                              (div: class: "qons-buttons"
                                                    (button: class: "primary"
                                                             onclick: "window.location.href='/r/' + document.querySelector('[name=roomId]').value"
                                                             type: "button"
                                                             "Join")
                                                    (button: hx-post: "/r"
                                                             hx-swap: "none"
                                                             hx-prompt: "Name your new room:"
                                                             type: "button"
                                                             class: "secondary"
                                                             "Create Room"))))
                                   (section: class: "invis-container desc"
                                             (h3: "Ask answerable questions, get questionable answers")
                                             (p: "Qons is an anonymous (name optional) Q&A application similar to sli.do")
                                             (p: "On your first visit, you will be assigned a session ID. Creating a room gives you an access to an admin link. Anyone who clicks this link becomes an admin to this room. If you want to retain long-term admin access to rooms, save this link somewhere safe.")
                                             (p: "You can lock and unlock rooms as neccessary. Each user can only upvote once.")
                                             (p: "Inactive rooms are periodically pruned.")
                                             (p: "Qons is written in Scheme with HTMX and SQLite.")))
                             (div: style: "display: flex; flex-direction: column"
                                   (section: class: "container"
                                             (p: "You are the admin of the following rooms:")
                                             (ul: style: "display: flex; flex-direction: column; padding: 0"
                                                  class: "list-buttons"
                                                  ,@(map (lambda (room-pair)
                                                           (let ((id (car room-pair))
                                                                 (room (cdr room-pair)))
                                                             (shsx
                                                              (li: style: "list-style-type: none"
                                                                   (a: href: ,(format "/r/~a" id)
                                                                       class: "button"
                                                                       ,(substring (room-name room) 0 10))))))
                                                         admin-rooms)))
                                   (section: class: "container" style: "margin-top: 1rem"
                                             (p: "Recently visited rooms:")
                                             (ul: style: "display: flex; flex-direction: column; padding: 0"
                                                  class: "list-buttons"
                                                  ,@(map (lambda (room)
                                                           (shsx
                                                            (li: style: "list-style-type: none"
                                                                 (a: href: ,(format "/r/~a" (room-id room))
                                                                     class: "button"
                                                                     ,(substring (room-name room) 0 10)))))
                                                         recent-rooms)))))
                       (footer: "Made anno domini 2025, Lukáš Hozda"))))))


;; Room page - question form
(define (question-form room-id locked?)
  (shsx
   (div: id: "question-form"
         ,(@if locked?
            (p: class: "locked-message" "This room is locked - no new questions can be added")
            (form: hx-post: ,(format "/r/~a/questions" (number->string room-id))
                   hx-swap: "none"
                   hx-target: "#questions"
                   (dis: style: "display: flex; flex-direction: row;"
                         (textarea: style: "flex: 1"
                                    name: "text"
                                    id: "question-input"
                                    placeholder: "Ask a question..."
                                    required: ""))
                   (div: style: "display: flex; flex-direction: row;"
                         (input: type: "text"
                                 name: "author"
                                 style: "flex: 1"
                                 placeholder: "Enter your name (optional)")
                         (button: "Ask")))))))

;; Room page - main template
(define (room-page room questions admin-status?)
  (base-template
   (format "~a #~a" (room-name room) (room-id room))
   (shsx
    (main: class: "invis-container"
           (a: href: "/"
               class: "qons-title"
               "'(Q0NS? ...)")
           (section: class: "container"
                     (div: class: "q-title"
                           (div:
                            (span: class: "q-title-text" ,(format "~a #" (room-name room)))
                            (span: class: "q-title-id"   id: "room-id" ,(number->string (room-id room))))
                           (div: class: "small-buttons"
                                 (button:
                                  onclick: "navigator.clipboard.writeText(window.location.href).then(() => alert('Link copied to clipboard!'))"
                                  "🔗 Share")
                                 ,(@when admin-status?
                                    (button: hx-delete: ,(format "/r/~a" (room-id room))
                                             hx-confirm: "Delete this room and all its questions?"
                                             hx-target: "body"
                                             hx-push-url: "/"
                                             "🗑️ Delete"))))
                     ,(@when admin-status?
                        (p: "You are the admin of this room. Copy its admin link "
                            (a: href: ,(format "/r/~a/~a"
                                               (number->string (room-id room))
                                               (room-admin-token room))
                                "here")
                            "."
                            (div: style: "display: flex"
                                  (span: style: "margin-right: 0.5rem" "Lock the room: ")
                                  (label: class: "switch"
                                          (input: type: "checkbox"
                                                  ,@(if (eqv? 1 (room-locked room))
                                                      '(checked: "true")
                                                      '())
                                                  hx-post: ,(format "/r/~a/lock" (room-id room))
                                                  hx-swap: "none"
                                                  hx-vals: "js:{locked: event.target.checked}")
                                          (span: class: "switch-slider"
                                                 (div:))))))
                     ,(question-form (room-id room) (eqv? 1 (room-locked room)))
                     ,(questions-list room questions))))))

;; Questions list partial (for HTMX updates)
(define (questions-list room questions-with-votes)
  (shsx
   (div: id: "questions"
         hx-get: ,(format "/r/~a/questions" (room-id room))
         hx-trigger: "every 10s, questionsModified from:body"
         hx-swap: "outerHTML swap:*"
         class: "questions"
         ,@(map (lambda (qvi)
                  (question-item (room-id room)
                                 (car qvi)      ; question
                                 (cadr qvi)     ; votes
                                 (caddr qvi)    ; voted?
                                 (cadddr qvi)   ; is-admin?
                                 (room-locked room))) ; pass locked status
                questions-with-votes))))


;; Single question item
(define (question-item room-id q votes voted-num is-admin? locked-num)
  (define voted? (not (= voted-num 0)))
  (define locked? (= locked-num 1))
  (shsx
   (div: class: "question container"
         style: "display: flex; flex-direction: row"
         id: ,(format "q-~a" (question-id q))
         (div: style: "flex: 1"
               (div: style: "display: flex;"
                     ,(@when (question-author q)
                        (small: class: "q-author" ,(string-append "$ " (question-author q))))
                     (span: class: "vote-count"
                            ,(number->string votes) " " ,(if (= votes 1) "vote" "votes")))
               (p: style: "word-break: break-word;"
                   ,(question-text q)))
         (div: class: "question-controls" style: "display: flex; flex-direction: column; align-items: flex-end"
               ,(@unless locked? ; Only show vote button if room not locked
                  (button: hx-post: ,(format "/r/~a/questions/~a/~a"
                                             room-id
                                             (question-id q)
                                             (if voted? "down" "up"))
                           hx-swap: "none"
                           class: ,(string-append "upvote"
                                                  (if voted? " active" ""))
                           ,(if voted? "▼" "▲")))
               ,(@when is-admin?
                  (button: hx-delete: ,(format "/r/~a/questions/~a"
                                               room-id
                                               (question-id q))
                           hx-swap: "none"
                           hx-confirm: "Remove this question?"
                           class: "delete"
                           "✓"))))))
