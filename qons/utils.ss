;;; -*- Gerbil -*-
(import :std/sugar
        :std/format
        :std/crypto
        :std/misc/bytes)
(export #t)

(define (random-integer cap)
  (modulo (u8vector->uint (random-bytes 12))
          cap))
