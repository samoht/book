(jbuild_version 1)

(alias (
  (name code)
  (deps (polymorphism.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? polymorphism.topscript polymorphism.topscript.corrected)))))

(alias (
  (name code)
  (deps (stack.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? stack.topscript stack.topscript.corrected)))))

(alias (
  (name code)
  (deps (subtyping.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? subtyping.topscript subtyping.topscript.corrected)))))

(alias (
  (name code)
  (deps (immutable.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? immutable.topscript immutable.topscript.corrected)))))

(alias (
  (name code)
  (deps (row_polymorphism.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? row_polymorphism.topscript row_polymorphism.topscript.corrected)))))

