(jbuild_version 1)

(alias (
  (name cram)
  (deps (warn_help.sh))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe warn_help.sh)
    (diff? warn_help.sh          warn_help.sh.corrected)))))

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
  (deps (main2.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose main2.topscript))
    (diff? main2.topscript main2.topscript.corrected)))))

