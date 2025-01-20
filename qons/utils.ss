;;; -*- Gerbil -*-
(import :std/sugar
        :std/format
        :std/crypto
        :std/misc/bytes
        :lho/fxns/lib)
(export #t)

(fn :ret random-integer ((cap : integer?) -> integer?)
    (modulo (u8vector->uint (random-bytes 12))
            cap))
