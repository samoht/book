(jbuild_version 1)

(alias (
  (name code)
  (deps (blang.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose blang.topscript))
    (diff? blang.topscript blang.topscript.corrected)))))

(alias (
  (name code)
  (deps (logger.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose logger.topscript))
    (diff? logger.topscript logger.topscript.corrected)))))

(alias (
  (name code)
  (deps (main.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose main.topscript))
    (diff? main.topscript main.topscript.corrected)))))

(alias (
  (name code)
  (deps (catch_all.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose catch_all.topscript))
    (diff? catch_all.topscript catch_all.topscript.corrected)))))

