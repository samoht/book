(jbuild_version 1)

(alias (
  (name code)
  (deps (iter.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose iter.topscript))
    (diff? iter.topscript iter.topscript.corrected)))))

(alias (
  (name code)
  (deps (binary.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose binary.topscript))
    (diff? binary.topscript binary.topscript.corrected)))))

(alias (
  (name code)
  (deps (istack.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose istack.topscript))
    (diff? istack.topscript istack.topscript.corrected)))))

(alias (
  (name code)
  (deps (initializer.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose initializer.topscript))
    (diff? initializer.topscript initializer.topscript.corrected)))))

(alias (
  (name code)
  (deps (stack.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose stack.topscript))
    (diff? stack.topscript stack.topscript.corrected)))))

