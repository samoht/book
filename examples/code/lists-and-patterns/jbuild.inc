(jbuild_version 1)

(alias (
  (name code)
  (deps (poly.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose poly.topscript))
    (diff? poly.topscript poly.topscript.corrected)))))

(alias (
  (name code)
  (deps (main.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose main.topscript))
    (diff? main.topscript main.topscript.corrected)))))

