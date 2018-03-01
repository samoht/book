(jbuild_version 1)

(alias (
  (name code)
  (deps (main.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? main.topscript main.topscript.corrected)))))

(alias (
  (name code)
  (deps (intro.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? intro.topscript intro.topscript.corrected)))))

