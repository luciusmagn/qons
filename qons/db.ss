;;; -*- Gerbil -*-
(import :std/db/dbi
        :std/db/sqlite
        :std/db/conpool
        :std/sugar
        :std/format
        :std/error
        :std/srfi/19
        :lho/fxns/lib
        ./lib)
(export init-db!
        with-db
        create-room!
        set-room-lock!
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

(fn :ret init-db! ((db-path : string?) -> void?)
    (unless db-pool
      (set! db-pool
        (make-conpool
         (cut sql-connect sqlite-open db-path)
         20)))  ; max 20 concurrent connections
    (create-tables!))

;; Helper function for DB operations
(fn :ret with-db ((fn : procedure?) -> any?)
    (unless db-pool
      (printf "You forgot to initialize the DB pool. Use (init-db! db-path)\n"))
    (let ((conn (conpool-get db-pool 20)))
      (try (fn conn)
           (catch (e)
             (conpool-release db-pool conn)
             (raise e))
           (finally (conpool-put db-pool conn)))))

;; Initialize tables
(fn :ret create-tables! (-> void?)
    (with-db
     (lambda (conn)
       (sql-eval conn "CREATE TABLE IF NOT EXISTS room (
                       id INTEGER PRIMARY KEY,
                       admin_token TEXT NOT NULL,
                       name TEXT DEFAULT NULL,
                       locked BOOLEAN NOT NULL DEFAULT 0,
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
                       )")))
    (void))

;; Room operations
(fn :ret create-room! ((id : number?) (admin-token : string?) (name : string?) -> room?)
    (with-db
     (lambda (conn)
       (sql-eval conn
                 "INSERT INTO room (id, admin_token, name, locked, created_at) VALUES (?, ?, ?, 0, datetime('now'))"
                 id admin-token name)
       (get-room id))))

(fn :ret set-room-lock! ((id : number?) (admin-token : string?) (locked? : integer?) -> void?)
    (with-db
     (lambda (conn)
       (sql-eval conn
                 "UPDATE room SET locked = ? WHERE id = ? AND admin_token = ?"
                 (if locked? 1 0) id admin-token)))
    (void))

(fn :ret get-room ((id : number?) -> (or-t room? boolean?))
    (with-db
     (lambda (conn)
       (define stmt (sql-prepare conn "SELECT * FROM room WHERE id = ?"))
       (sql-bind stmt id)

       (define result (sql-query stmt))

       (if (null? result) #f
           (let ((r (car result)))
             (room (vector-ref r 0)
                   (vector-ref r 1)
                   (vector-ref r 2)
                   (vector-ref r 3)
                   (vector-ref r 4)))))))

(fn :ret delete-room! ((id : number?) (admin-token : string?) -> void?)
    (with-db
     (lambda (conn)
       (sql-eval conn
                 "DELETE FROM room WHERE id = ? AND admin_token = ?"
                 id admin-token)))
    (void))

;; Question operations
(fn :ret create-question! ((room-id : number?) (text : string?) (author : string?) -> void?)
    (with-db
     (lambda (conn)
       (sql-eval conn
                 "INSERT INTO question (room_id, text, author, created_at)
               VALUES (?, ?, ?, datetime('now'))"
                 room-id text author)))
    (void))

(fn :ret get-room-questions ((room-id : number?) (session-id : string?) -> (list-of vector?))
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

(fn :ret delete-question! ((id : number?) (room-id : number?) -> void?)
    (with-db
     (lambda (conn)
       (sql-eval conn
                 "DELETE FROM question WHERE id = ? AND room_id = ?"
                 id room-id)))
    (void))

;; Vote operations
(fn :ret create-vote! ((session-id : string?) (question-id : number?) -> void?)
    (with-db
     (lambda (conn)
       (try
        (sql-eval conn
                  "INSERT INTO vote (session_id, question_id, created_at)
                VALUES (?, ?, datetime('now'))"
                  session-id question-id)
        (catch (e) #f))))  ; silently fail on duplicate votes
    (void))

(fn :ret delete-vote! ((session-id : string?) (question-id : number?) -> void?)
    (with-db
     (lambda (conn)
       (sql-eval conn
                 "DELETE FROM vote
               WHERE session_id = ? AND question_id = ?"
                 session-id question-id)))
    (void))

(fn :ret get-question-votes ((question-id : number?) -> number?)
    (with-db
     (lambda (conn)
       (define result
         (sql-eval-query conn
                         "SELECT COUNT(*) FROM vote WHERE question_id = ?"
                         question-id))
       (if (null? result) 0
           (vector-ref (car result) 0)))))
