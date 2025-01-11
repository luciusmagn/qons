;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/cli/getopt
        ./lib)
(export main)

;; build manifest; generated during the build
;; defines version-manifest which you can use for exact versioning
(include "../manifest.ss")

(def (main . args)
  (call-with-getopt qons-main args
    program: "qons"
    help: "A one line description of your program"
    ;; commands/options/flags for your program; see :std/cli/getopt
    ;; ...
    ))

(def* qons-main
  ((opt)
   (qons-main/options opt))
  ((cmd opt)
   (qons-main/command cmd opt)))

;;; Implement this if your CLI doesn't have commands
(def (qons-main/options opt)
  (error "Implement me!"))

;;; Implement this if your CLI has commands
(def (qons-main/command cmd opt)
  (error "Implement me!"))
