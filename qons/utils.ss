;;; -*- Gerbil -*-
(import :std/sugar
        :std/format
        :std/crypto
        :std/misc/bytes
        :std/text/json
        :lho/fxns/lib
        :lho/smart-httpd/lib
        ./db
        ./lib)
(export #t)

(fn :ret random-integer ((cap : integer?) -> integer?)
    (modulo (u8vector->uint (random-bytes 12))
            cap))

(define (is-admin? room-id cookies)
  (let* ((admin-cookie (find-cookie-val cookies "admin_rooms"))
         (admin-rooms  (if admin-cookie
                         (try (string->json-object admin-cookie)
                              (catch (e) (hash)))
                         (hash)))
         (room-token   (hash-ref admin-rooms (string->symbol (number->string room-id)) #f)))
    (and room-token
         (let ((room (get-room room-id)))
           (and room
                (equal? room-token (room-admin-token room)))))))
