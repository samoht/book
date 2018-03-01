(jbuild_version 1)

(alias (
  (name code)
  (deps (core_phys_equal.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? core_phys_equal.topscript core_phys_equal.topscript.corrected)))))

(alias (
  (name code)
  (deps (main.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? main.topscript main.topscript.corrected)))))

