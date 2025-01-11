;;; -*- Gerbil -*-
(import :std/sugar
        :std/format
        :std/misc/uuid
        :std/text/json
        :lho/shsx/lib
        :lho/smart-httpd/lib
        ./lib
        ./view)
(export run-api)

;; Index handler - sets session cookie if not present
(def index-handler
  (handler () <- (_ :>)
           (list 200
                 (list (cons "Set-Cookie" "session_id=..."))
                 (render-html (index-page)))))

;; Create room
(def create-room-handler
  (handler () <- (_ :>)
           ;; TODO: generate room id and admin token
           (list 302
                 (list (cons "Location" "/r/123/abc"))
                 "")))

;; View room (user view)
(def view-room-handler
  (handler ((id :>number)) <- (_ :>)
           ;; TODO: fetch room from DB
           (let ((room (room 123 "abc" #f)))
             (cons 200 (render-html (room-page room))))))

;; Admin access to room
(def admin-room-handler
  (handler ((id :>number) (token :>string)) <- (_ :>)
           ;; TODO: verify token and set admin cookie
           (list 302
                 (list (cons "Location" "/r/123")
                       (cons "Set-Cookie" "admin_rooms={...}"))
                 "")))

;; Get questions (polling)
(def get-questions-handler
  (handler ((id :>number)) <- (_ :>)
           ;; TODO: fetch questions from DB
           (let ((room (room id "abc" #f)))
             (cons 200 (render-html (questions-list room '()))))))

;; Rest of handlers unchanged since they return plain strings
(def submit-question-handler
  (handler ((id :>number)) <- (body :>form)
           ;; TODO: save question to DB
           (cons 200 "")))

(def delete-question-handler
  (handler ((id :>number) (qid :>number)) <- (_ :>)
           ;; TODO: verify admin and delete
           (cons 200 "")))

(def upvote-handler
  (handler ((id :>number) (qid :>number)) <- (_ :>)
           ;; TODO: record vote in DB
           (cons 200 "")))

(def delete-room-handler
  (handler ((id :>number)) <- (_ :>)
           ;; TODO: verify admin and delete
           (cons 200 "")))

(def routes
  (list
   (get    "/"                        index-handler)
   (post   "/r"                       create-room-handler)
   (get    "/r/:id"                   view-room-handler)
   (get    "/r/:id/:token"            admin-room-handler)
   (get    "/r/:id/questions"         get-questions-handler)
   (post   "/r/:id/questions"         submit-question-handler)
   (delete "/r/:id/questions/:qid"    delete-question-handler)
   (post   "/r/:id/questions/:qid/up" upvote-handler)
   (delete "/r/:id"                   delete-room-handler)))

(def (run-api port host)
  (run-server routes port: port address: host))
