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

;; TODO: move elswhere, perhaps?
(define (is-admin? room-id cookies)
  (displayln "in is-admin?")
  (let* ((admin-cookie (find-cookie-val cookies "admin_rooms"))
         (ieaie        (displayln "found admin_rooms"))
         (admin-rooms  (if admin-cookie
                         (try (string->json-object admin-cookie)
                              (catch (e) (hash)))
                         (hash)))
         (ieaoooo      (displayln admin-rooms))
         (room-token   (hash-ref admin-rooms (number->string room-id) #f)))
    (displayln "got a smelly token")
    (and room-token
         (let ((room (get-room room-id)))
           (and room
                (equal? room-token (room-admin-token room)))))))

;; Index handler - sets session cookie if not present
(define index-handler
  (handler ((cookies :>cookies)) <- (body :>)
           (let ((session-id (or (find-cookie-val cookies "session_id")
                                 (uuid->string (random-uuid)))))
             (respond-with
              (:status 200)
              (:header "HX-Refresh" "true")
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
(define view-room-handler
  (handler ((id :>number) (cookies :>cookies)) <- (_ :>)
           (let ((room (get-room id)))
             (if room
               (let ((admin-status (is-admin? id cookies)))
                 (respond-with
                  (:status 200)
                  (:body (render-html (room-page room admin-status)))))
               (respond-with
                (:status 404)
                (:body "No such room foo"))))))

;; Admin access to room
(define admin-room-handler
  (handler ((id :>number) (token :>string) (cookies :>cookies)) <- (_ :>)
           (let ((room (get-room id)))
             (cond
              ((not room)
               (cons 404 "Room not found"))

              ((not (equal? token (room-admin-token room)))
               (cons 403 "Invalid admin token"))

              (else
               ;; Get existing admin rooms from cookie
               (let* ((admin-cookie (find-cookie-val cookies "admin_rooms"))
                      (admin-rooms (if admin-cookie
                                     (try (string->json-object admin-cookie)
                                          (catch (e) (hash)))
                                     (hash)))
                      ;; Add this room to admin hash
                      (new-admin-rooms
                       (begin (hash-put! admin-rooms
                                         (number->string id)
                                         token)
                              admin-rooms)))
                 (displayln "FUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUCK")
                 (displayln new-admin-rooms)
                 (respond-with
                  (:status 200)
                  (:header "HX-Redirect" (format "/r/~a" id))
                  (:cookie "admin_rooms"
                   (call-with-output-string
                     (cut write-json new-admin-rooms <>)))
                  (:body ""))))))))

;; Get questions (polling)
(define get-questions-handler
  (handler ((id :>number)) <- (_ :>)
           ;; TODO: fetch questions from DB
           (let ((room (room id "abc" #f)))
             (cons 200 (render-html (questions-list room '()))))))

;; Rest of handlers unchanged since they return plain strings
(define submit-question-handler
  (handler ((id :>number)) <- (body :>form)
           ;; TODO: save question to DB
           (cons 200 "")))

(define delete-question-handler
  (handler ((id :>number) (qid :>number)) <- (_ :>)
           ;; TODO: verify admin and delete
           (cons 200 "")))

(define upvote-handler
  (handler ((id :>number) (qid :>number)) <- (_ :>)
           ;; TODO: record vote in DB
           (cons 200 "")))

(define delete-room-handler
  (handler ((id :>number)) <- (_ :>)
           ;; TODO: verify admin and delete
           (cons 200 "")))

(define routes
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

(define (run-api port host)
  (run-server routes
              port: port
              address: host))
