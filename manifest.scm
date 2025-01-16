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
           (pid-file "/tmp/dev-server.pid"))

       (use-modules (ice-9 popen)
                    (ice-9 rdelim))

       (letrec ((kill-server
                 (lambda ()
                   (when (file-exists? pid-file)
                     (system* "kill" (call-with-input-file pid-file read-line))
                     (delete-file pid-file))))

                (start-server
                 (lambda ()
                   (kill-server)
                   (system* "sh" "-c"
                            (string-append binary-path " & echo $! > " pid-file))))

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
