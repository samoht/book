(jbuild_version 1)

(alias (
  (name code)
  (deps (basic.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? basic.topscript basic.topscript.corrected)))))

(alias (
  (name code)
  (deps (step.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? step.topscript step.topscript.corrected)))))

(alias (
  (name code)
  (deps (command_types.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? command_types.topscript command_types.topscript.corrected)))))

(alias (
  (name code)
  (deps (group.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? group.topscript group.topscript.corrected)))))

