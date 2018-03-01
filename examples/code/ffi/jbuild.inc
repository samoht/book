(jbuild_version 1)

(alias (
  (name code)
  (deps (qsort.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? qsort.topscript qsort.topscript.corrected)))))

(alias (
  (name code)
  (deps (posix.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? posix.topscript posix.topscript.corrected)))))

