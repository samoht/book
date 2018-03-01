(jbuild_version 1)

(alias (
  (name code)
  (deps (query_handler.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? query_handler.topscript query_handler.topscript.corrected)))))

(alias (
  (name code)
  (deps (main.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? main.topscript main.topscript.corrected)))))

