#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

;;(defbuild-script
;;  '("qons/lib"
;;    (exe: "qons/main" bin: "qons")))
(defbuild-script
  '("qons/lib" "qons/view" "qons/db" "qons/http"
    (exe: "qons/main" bin: "qons")))
