(jbuild_version 1)

(alias (
  (name code)
  (deps (basic.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose basic.topscript))
    (diff? basic.topscript basic.topscript.corrected)))))

(alias (
  (name code)
  (deps (step.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose step.topscript))
    (diff? step.topscript step.topscript.corrected)))))

(alias (
  (name code)
  (deps (command_types.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose command_types.topscript))
    (diff? command_types.topscript command_types.topscript.corrected)))))

(alias (
  (name code)
  (deps (group.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose group.topscript))
    (diff? group.topscript group.topscript.corrected)))))

