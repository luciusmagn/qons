;;; -*- Gerbil -*-
(import :std/db/dbi
        :std/db/sqlite
        :std/db/conpool
        :std/sugar
        :std/format
        :std/error
        :std/srfi/19
        ./lib)
(export init-db!
        with-db
        create-room!
        get-room
        delete-room!
        create-question!
        get-room-questions
        delete-question!
        create-vote!
        delete-vote!
        get-question-votes)

;; Connection pool management
(define db-pool #f)

(define (init-db! db-path)
  (unless db-pool
    (set! db-pool
      (make-conpool
       (cut sql-connect sqlite-open db-path)
       20)))  ; max 20 concurrent connections
  (create-tables!))

;; Helper function for DB operations
(define (with-db fn)
  (unless db-pool
    (printf "You forgot to initialize the DB pool. Use (init-db! db-path)\n"))
  (let ((conn (conpool-get db-pool 20)))
    (displayln "connection acquired")
    (try (fn conn)
         (catch (e)
           (conpool-release db-pool conn)
           (raise e))
         (finally (conpool-put db-pool conn)))))

;; Initialize tables
(define (create-tables!)
  (with-db
   (lambda (conn)
     (sql-eval conn "CREATE TABLE IF NOT EXISTS room (
                    id INTEGER PRIMARY KEY,
                    admin_token TEXT NOT NULL,
                    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
                    )")

     (sql-eval conn "CREATE TABLE IF NOT EXISTS question (
                    id INTEGER PRIMARY KEY AUTOINCREMENT,
                    room_id INTEGER NOT NULL,
                    text TEXT NOT NULL,
                    author TEXT,
                    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (room_id) REFERENCES room(id) ON DELETE CASCADE
                    )")

     (sql-eval conn "CREATE INDEX IF NOT EXISTS idx_question_room
               ON question(room_id)")

     (sql-eval conn "CREATE TABLE IF NOT EXISTS vote (
                    session_id TEXT NOT NULL,
                    question_id INTEGER NOT NULL,
                    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                    PRIMARY KEY (session_id, question_id),
                    FOREIGN KEY (question_id) REFERENCES question(id) ON DELETE CASCADE
                    )"))))

;; Room operations
(define (create-room! id admin-token)
  (with-db
   (lambda (conn)
     (sql-eval conn
               "INSERT INTO room (id, admin_token, created_at) VALUES (?, ?, datetime('now'))"
               id admin-token)
     (get-room id))))

(define (get-room id)
  (with-db
   (lambda (conn)
     (define stmt (sql-prepare conn "SELECT * FROM room WHERE id = ?"))
     (sql-bind stmt id)

     (define result (sql-query stmt))

     (if (null? result) #f
         (let ((r (car result)))
           (room (vector-ref r 0)
                 (vector-ref r 1)
                 (vector-ref r 2)))))))

(define (delete-room! id admin-token)
  (with-db
   (lambda (conn)
     (sql-eval conn
               "DELETE FROM room WHERE id = ? AND admin_token = ?"
               id admin-token))))

;; Question operations
(define (create-question! room-id text author)
  (with-db
   (lambda (conn)
     (sql-eval conn
               "INSERT INTO question (room_id, text, author, created_at)
               VALUES (?, ?, ?, datetime('now'))"
               room-id text author))))

(define (get-room-questions room-id session-id)
  (with-db
   (lambda (conn)
     (define stmt
       (sql-prepare conn
                    "SELECT q.*, COUNT(v.question_id) as votes,
                    EXISTS(
                      SELECT 1 FROM vote
                      WHERE question_id = q.id
                      AND session_id = ?
                    ) as voted_by_user
                    FROM question q
                    LEFT JOIN vote v ON v.question_id = q.id
                    WHERE q.room_id = ?
                    GROUP BY q.id
                    ORDER BY votes DESC, created_at DESC"))
     (sql-bind stmt session-id room-id)
     (sql-query stmt))))

(define (delete-question! id room-id)
  (with-db
   (lambda (conn)
     (sql-eval conn
               "DELETE FROM question WHERE id = ? AND room_id = ?"
               id room-id))))

;; Vote operations
(define (create-vote! session-id question-id)
  (with-db
   (lambda (conn)
     (try
      (sql-eval conn
                "INSERT INTO vote (session_id, question_id, created_at)
                VALUES (?, ?, datetime('now'))"
                session-id question-id)
      (catch (e) #f)))))  ; silently fail on duplicate votes

(define (delete-vote! session-id question-id)
  (with-db
   (lambda (conn)
     (sql-eval conn
               "DELETE FROM vote
               WHERE session_id = ? AND question_id = ?"
               session-id question-id))))

(define (get-question-votes question-id)
  (with-db
   (lambda (conn)
     (define result
       (sql-eval-query conn
                       "SELECT COUNT(*) FROM vote WHERE question_id = ?"
                       question-id))
     (if (null? result) 0
         (vector-ref (car result) 0)))))

;; Room methods
(define (room.save! self)
  (create-room! (room-id self)
                (room-admin-token self)))

(define (room.delete! self)
  (delete-room! (room-id self)
                (room-admin-token self)))

(define (room.get id)
  (get-room id))

(define (room.questions self session-id)
  (get-room-questions (room-id self) session-id))

;; Question methods
(define (question.save! self)
  (create-question! (question-room-id self)
                    (question-text self)
                    (question-author self)))

(define (question.delete! self)
  (delete-question! (question-id self)
                    (question-room-id self)))

(define (question.votes self)
  (get-question-votes (question-id self)))

;; Vote methods
(define (vote.save! self)
  (create-vote! (vote-session-id self)
                (vote-question-id self)))
