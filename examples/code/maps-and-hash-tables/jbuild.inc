(jbuild_version 1)

(alias (
  (name code)
  (deps (core_phys_equal.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose core_phys_equal.topscript))
    (diff? core_phys_equal.topscript core_phys_equal.topscript.corrected)))))

(alias (
  (name code)
  (deps (main.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose main.topscript))
    (diff? main.topscript main.topscript.corrected)))))

