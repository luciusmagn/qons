;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/cli/getopt
        :lho/shsx/lib
        :lho/smart-httpd/lib
        ./lib
        ./http
        ./db)
(export main)

;; build manifest; generated during the build
(include "../manifest.ss")

(define (qons-main opt)
  (let ((port (hash-ref opt 'port "8080"))
        (host (hash-ref opt 'host "127.0.0.1"))
        (db-path (hash-ref opt 'db "qons.db")))
    (displayln "Initializing database at " db-path)
    (init-db! db-path)
    (displayln "Starting QONS on " host ":" port)
    (run-api port host)))

;; Add database option to main
(define (main . args)
  (call-with-getopt qons-main args
                    program: "qons"
                    help: "QONS - Question Room Server"
                    (option 'port "-p" "--port"
                            help: "Port to listen on"
                            default: "8080")
                    (option 'host "-h" "--host"
                            help: "Host address to bind to"
                            default: "127.0.0.1")
                    (option 'db "-d" "--db"
                            help: "SQLite database path"
                            default: "qons.db")))
