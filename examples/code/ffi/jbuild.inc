(jbuild_version 1)

(alias (
  (name code)
  (deps (qsort.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose qsort.topscript))
    (diff? qsort.topscript qsort.topscript.corrected)))))

(alias (
  (name code)
  (deps (posix.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose posix.topscript))
    (diff? posix.topscript posix.topscript.corrected)))))

