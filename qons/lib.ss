;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/srfi/9)
(export #t)

(define-record-type <room>
  (room id admin-token name locked created-at)
  room?
  (id          room-id)
  (admin-token room-admin-token)
  (name        room-name)
  (locked      room-locked)
  (created-at  room-created-at))

(define-record-type <question>
  (question id room-id text author created-at)
  question?
  (id         question-id)
  (room-id    question-room-id)
  (text       question-text)
  (author     question-author)
  (created-at question-created-at))

(define-record-type <vote>
  (vote session-id question-id created-at)
  vote?
  (session-id  vote-session-id)
  (question-id vote-question-id)
  (created-at  vote-created-at))
