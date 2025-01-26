;;; -*- Gerbil -*-
(import :std/sugar
        :std/format
        :std/crypto
        :std/misc/bytes
        :std/text/json
        :std/srfi/1
        :std/srfi/125
        :lho/fxns/lib
        :lho/smart-httpd/lib
        ./db
        ./lib)
(export #t)

;; Thank you, Lassi Kortela :)!
(define (remove-duplicates xs)
  (let ((seen (make-hash-table equal?)))
    (let loop ((xs xs) (new-list '()))
      (if (null? xs)
        (reverse new-list)
        (loop (cdr xs)
              (let ((x (car xs)))
                (if (hash-table-contains? seen x)
                  new-list
                  (begin (hash-table-set! seen x #t)
                         (cons x new-list)))))))))

;; shame on me for thinking this will be the same as Rust
(fn :ret merciful-take ((lst : list?) (count : number?) -> list?)
    (if (>= (length lst) count)
      (take lst count)
      lst))

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

(define max-recent-rooms 5)
(fn :ret update-recent-rooms ((new-id : number?) (cookies : (list-of request-cookie?)) -> (list-of room?))
    (let* ((recent-cookie (find-cookie-val cookies "recent_rooms"))
           (recent-ids    (if recent-cookie
                            (try (map string->number
                                      (string->json-object recent-cookie))
                                 (catch (e)
                                   (displayln e)
                                   '()))
                            '()))
           ;; Remove duplicates and add new id at front
           (updated-ids (merciful-take (remove-duplicates
                                        (cons new-id recent-ids))
                                       max-recent-rooms))
           ;; Get room details for each id
           (rooms       (filter-map get-room updated-ids)))
      rooms))

(fn :ret rooms->cookie ((rooms : (list-of room?)) -> string?)
    (call-with-output-string
     (cut write-json
          (list->vector
           (map (lambda (r) (number->string (room-id r)))
                rooms))
          <>)))
