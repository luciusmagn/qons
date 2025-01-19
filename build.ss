#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("qons/lib" "qons/view" "qons/db" "qons/http" "qons/utils"
    (exe: "qons/main" bin: "qons")))
