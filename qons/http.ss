;;; -*- Gerbil -*-
(import :std/sugar
        :std/format
        :std/misc/uuid
        :std/text/json
        :lho/shsx/lib
        :lho/smart-httpd/lib
        (only-in :std/srfi/1 find)
        ./lib
        ./view
        ./utils
        ./db)
(export #t)

;; Index handler - sets session cookie if not present
(def index-handler
  (handler ((cookies :>cookies)) <- (body :>)
           (let ((session-id (or (find-cookie-val cookies "session_id")
                                 (uuid->string (random-uuid)))))
             (respond-with
              (:status 200)
              (:cookie "session_id" session-id)
              (:body (render-html (index-page)))))))

;; Create room
(define create-room-handler
  (handler () <- (_ :>)
           (define (random-room-id)
             ;; generate id between 100000 and 999999
             (+ 100000 (random-integer 1000000)))

           (define (random-token)
             (define chars "0123456789abcdef")
             (define len   (string-length chars))

             (let loop ((n 12) (acc ""))
               (if (zero? n)
                 acc
                 (loop (- n 1)
                       (string-append
                        acc
                        (string (string-ref chars (random-integer len))))))))

           (let* ((id    (random-room-id))
                  (token (random-token))
                  (room  (create-room! id token)))
             (respond-with
              (:status 302)
              (:header "Location" (format "/r/~a/~a" id token))
              (:body "")))))

;; View room (user view)
(def view-room-handler
  (handler ((id :>number)) <- (_ :>)
           ;; TODO: fetch room from DB
           (let ((room (room 123 "abc" "2025-01-12")))
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
