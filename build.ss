#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("qons/lib"
    (exe: "qons/main" bin: "qons")))
