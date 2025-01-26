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

;; index handler just sets session cookie if not present
(define index-handler
  (handler ((cookies :>cookies)) <- (body :>)
           (let* ((session-id    (or (find-cookie-val cookies "session_id")
                                     (uuid->string (random-uuid))))
                  (admin-cookie  (find-cookie-val cookies "admin_rooms"))
                  (recent-cookie (find-cookie-val cookies "recent_rooms"))
                  (admin-rooms   (if admin-cookie
                                   (try (string->json-object admin-cookie)
                                        (catch (e) (hash)))
                                   (hash)))
                  (recent-ids    (if recent-cookie
                                   (try (map string->number
                                             (string->json-object recent-cookie))
                                        (catch (e) '()))
                                   '()))
                  (recent-rooms  (filter-map get-room recent-ids))
                  (admin-room-details
                   (filter-map (lambda (room-pair)
                                 (let* ((id (string->number (symbol->string (car room-pair))))
                                        (room (get-room id)))
                                   (and room (cons id room))))
                               (hash->list admin-rooms))))
             (respond-with
              (:status 200)
              (:header "HX-Refresh" "true")
              (:cookie "session_id" session-id)
              (:body (render-html (index-page admin-room-details recent-rooms)))))))

;; Create room
(define create-room-handler
  (handler ((headers :>headers)) <- (_ :>)
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

           (let* ((id     (random-room-id))
                  (token  (random-token))
                  (prompt (find (lambda (h) (equal? (car h) "Hx-Prompt")) headers))
                  (deaaa  (displayln "creating room"))
                  (room   (create-room! id token (if prompt
                                                   (cdr prompt)
                                                   "Unnamed room"))))
             (displayln "eeeeeeee")
             (respond-with
              (:status 302)
              (:header "Location" (format "/r/~a/~a" id token))
              (:body "")))))

;; View room (user view)
(define view-room-handler
  (handler ((id :>number) (cookies :>cookies)) <- (_ :>)
           (let* ((session-id       (or (find-cookie-val cookies "session_id")
                                        (uuid->string (random-uuid))))
                  (room            (get-room id))
                  (questions       (get-room-questions id session-id))
                  (mapped-questions
                   (map (lambda (q)
                          (list (question (vector-ref q 0)
                                          (vector-ref q 1)
                                          (vector-ref q 2)
                                          (vector-ref q 3)
                                          (vector-ref q 4))
                                (vector-ref q 5)
                                (vector-ref q 6)
                                (is-admin? id cookies)))
                        questions))
                  (recent-rooms    (if room
                                     (update-recent-rooms id cookies)
                                     '())))
             (if room
               (let ((admin-status (is-admin? id cookies)))
                 (respond-with
                  (:status 200)
                  (:cookie "recent_rooms" (rooms->cookie recent-rooms))
                  (:body (render-html (room-page room mapped-questions admin-status)))))
               (respond-with
                (:status 404)
                (:body "No such room"))))))

;; Admin access to room
(define admin-room-handler
  (handler ((id :>number) (token :>string) (cookies :>cookies) (headers :>headers)) <- (_ :>)
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
                 (respond-with
                  (if (find (lambda (h) (equal? (car h) "Hx-Request")) headers)
                    (list (:status 200)
                          (:header "HX-Redirect" (format "/r/~a" id)))
                    (list (:status 302)
                          (:header "Location" (format "/r/~a" id))))
                  (:cookie "admin_rooms"
                   (call-with-output-string
                    (cut write-json new-admin-rooms <>)))
                  (:body ""))))))))

;; Get questions (polling)
(define get-questions-handler
  (handler ((id :>number) (cookies :>cookies)) <- (_ :>)
           (let* ((session-id (find-cookie-val cookies "session_id"))
                  (room (get-room id)))
             (if room
               (let* ((questions (get-room-questions id session-id))
                      (question-converter
                       (lambda (q)
                         (list (question (vector-ref q 0)
                                         (vector-ref q 1)
                                         (vector-ref q 2)
                                         (vector-ref q 3)
                                         (vector-ref q 4))
                               (vector-ref q 5)
                               (vector-ref q 6)
                               (is-admin? id cookies))))
                      (mapped-questions (map question-converter questions))
                      (questions-template (questions-list room mapped-questions)))
                 (respond-with
                  (:status 200)
                  (:header "Content-Type" "text/html")
                  (:body (string-append
                          (render-html questions-template)
                          "<div hx-swap-oob=\"true\" id=\"question-form\">"
                          (render-html (question-form (room-id room)
                                                      (= 1 (room-locked room))))
                          "</div>"))))
               (respond-with
                (:status 404)
                (:body "Room not found"))))))

;; Rest of handlers unchanged since they return plain strings
(define submit-question-handler
  (handler ((id :>number) (cookies :>cookies)) <- (body :>form)
           (let* ((text (hash-ref body 'text #f))
                  (author (hash-ref body 'author #f))
                  (room (get-room id)))
             (cond
              ((not room)
               (respond-with
                (:status 404)
                (:body "Room not found")))
              ((= 1 (room-locked room))
               (respond-with
                (:status 403)
                (:body "Room is locked")))
              ((not text)
               (respond-with
                (:status 400)
                (:body "Question text required")))
              (else
               (create-question! id text author)
               (respond-with
                (:status 200)
                (:header "Hx-Trigger" "questionsModified")
                (:body "")))))))

(define delete-question-handler
  (handler ((id :>number) (qid :>number) (cookies :>cookies)) <- (_ :>)
           (if (is-admin? id cookies)
             (begin
               (delete-question! qid id)
               (respond-with
                (:status 200)
                (:header "Hx-Trigger" "questionsModified")
                (:body "")))
             (respond-with
              (:status 403)
              (:body "Not authorized")))))

(define upvote-handler
  (handler ((id :>number) (qid :>number) (cookies :>cookies)) <- (_ :>)
           (let ((session-id (find-cookie-val cookies "session_id")))
             (if session-id
               (begin
                 (create-vote! session-id qid)
                 (respond-with
                  (:status 200)
                  (:header "Hx-Trigger" "questionsModified")
                  (:body "")))
               (respond-with
                (:status 400)
                (:body "No session"))))))

(define delete-vote-handler
  (handler ((id :>number) (qid :>number) (cookies :>cookies)) <- (_ :>)
           (let ((session-id (find-cookie-val cookies "session_id")))
             (if session-id
               (begin
                 (delete-vote! session-id qid)
                 (respond-with
                  (:status 200)
                  (:header "Hx-Trigger" "questionsModified")
                  (:body "")))
               (respond-with
                (:status 400)
                (:body "No session"))))))

(define delete-room-handler
  (handler ((id :>number) (cookies :>cookies)) <- (_ :>)
           (let ((room (get-room id)))
             (if (and room (is-admin? id cookies))
               (begin
                 (delete-room! id (room-admin-token room))
                 (respond-with
                  (:status 200)
                  (:header "HX-Redirect" "/")
                  (:body "")))
               (respond-with
                (:status 403)
                (:body "Not authorized"))))))

(define toggle-lock-handler
  (handler ((id :>number) (cookies :>cookies)) <- (body :>form)
           (let* ((locked? (equal? "true" (hash-ref body 'locked #f)))
                  (room (get-room id)))
             (if (and room (is-admin? id cookies))
               (begin
                 (set-room-lock! id (room-admin-token room) locked?)
                 (respond-with
                  (:status 200)
                  (:header "Hx-Trigger" "questionsModified")
                  (:body "")))
               (respond-with
                (:status 403)
                (:body "Not authorized"))))))

(define routes
  (list
   (get    "/"                          index-handler)
   (post   "/r"                         create-room-handler)
   (get    "/r/:id"                     view-room-handler)
   (post   "/r/:id/lock"                toggle-lock-handler)
   (get    "/r/:id/questions"           get-questions-handler)
   (post   "/r/:id/questions"           submit-question-handler)
   (delete "/r/:id/questions/:qid"      delete-question-handler)
   (post   "/r/:id/questions/:qid/up"   upvote-handler)
   (post   "/r/:id/questions/:qid/down" delete-vote-handler)
   (get    "/r/:id/:token"              admin-room-handler)
   ;; TODO: :token vs questions order shouldn't matter, impl in smart-httpd
   (delete "/r/:id"                     delete-room-handler)))

(define (run-api port host)
  (run-server routes
              port: port
              address: host))
