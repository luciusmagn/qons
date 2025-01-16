;; -*- geiser-scheme-implementation: guile -*-
(use-modules (guix packages)
             (gnu packages scheme)
             (gnu packages guile)
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
   (inputs (cons* guile-3.0 extra-inputs))  ; Always include guile + extras
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

(define develop-script
  (make-guile-script
   "develop"
   ''(let loop ()
       (display "Rebuilding...\n")
       (system* "gerbil" "build")
       (display "Running...\n")
       (system* (string-append ".gerbil/bin/" "your-app-name"))
       (sleep 2)
       (loop))))

(packages->manifest
 (list gerbil
       gambit-c
       develop-script))
