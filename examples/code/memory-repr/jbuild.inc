(jbuild_version 1)

(alias (
  (name code)
  (deps (simple_record.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose simple_record.topscript))
    (diff? simple_record.topscript simple_record.topscript.corrected)))))

(alias (
  (name code)
  (deps (reprs.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose reprs.topscript))
    (diff? reprs.topscript reprs.topscript.corrected)))))

