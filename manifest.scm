;; -*- geiser-scheme-implementation: guile -*-
(use-modules (guix packages)
             (gnu packages scheme)
             (gnu packages guile)
             (gnu packages linux)
             (guix gexp)
             (guix licenses)
             (guix build-system trivial)
             (guix build utils))

(define* (make-guile-script name script-body #:key (extra-inputs '()))
  (package
   (name name)
   (version "0.1")
   (source #f)
   (build-system trivial-build-system)
   (propagated-inputs (cons* guile-3.0 inotify-tools extra-inputs))  ; Changed inputs to propagated-inputs
   (arguments
    (list
     #:builder
     (with-imported-modules
      '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (mkdir-p (string-append #$output "/bin"))
          (call-with-output-file (string-append #$output "/bin/" #$name)
            (lambda (port)
              (display "#!/usr/bin/env guile\n!#\n" port)
              (write #$script-body port)))
          (chmod (string-append #$output "/bin/" #$name) #o755)))))
   (synopsis (string-append name " helper script"))
   (description "Generated helper script")
   (license expat)
   (home-page #f)))

(define (make-dev-script project-name)
  (make-guile-script
   "develop"
   `'(let ((project-dir ,(string-append "./" project-name))
           (binary-path ,(string-append ".gerbil/bin/" project-name))
           (server-pid #f))

       (use-modules (ice-9 popen)
                    (ice-9 rdelim))

       (letrec ((kill-server
                 (lambda ()
                   (when server-pid
                     (system* "kill" (number->string server-pid)))))

                (start-server
                 (lambda ()
                   (kill-server)
                   (let ((pid (primitive-fork)))
                     (if (zero? pid)
                         (begin
                           (system* binary-path)
                           (exit 0))
                         (set! server-pid pid)))))

                (rebuild-and-restart
                 (lambda ()
                   (display "Rebuilding...\n")
                   (system* "gerbil" "build")
                   (display "Restarting server...\n")
                   (start-server))))

         (rebuild-and-restart)

         (let ((port (open-input-pipe
                      (string-append "inotifywait -m -r -e modify,create,delete "
                                     project-dir))))
           (let loop ()
             (let ((line (read-line port)))
               (when (not (eof-object? line))
                 (rebuild-and-restart)
                 (loop)))))))))

(packages->manifest
 (list gerbil
       gambit-c
       (make-dev-script "qons")))
