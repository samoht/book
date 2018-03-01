(jbuild_version 1)

(alias (
  (name code)
  (deps (iter.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? iter.topscript iter.topscript.corrected)))))

(alias (
  (name code)
  (deps (binary.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? binary.topscript binary.topscript.corrected)))))

(alias (
  (name code)
  (deps (istack.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? istack.topscript istack.topscript.corrected)))))

(alias (
  (name code)
  (deps (initializer.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? initializer.topscript initializer.topscript.corrected)))))

(alias (
  (name code)
  (deps (stack.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? stack.topscript stack.topscript.corrected)))))

