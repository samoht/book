(jbuild_version 1)

(alias (
  (name code)
  (deps (inline_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? inline_sexp.topscript inline_sexp.topscript.corrected)))))

(alias (
  (name code)
  (deps (sexp_default.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? sexp_default.topscript sexp_default.topscript.corrected)))))

(alias (
  (name code)
  (deps (to_from_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? to_from_sexp.topscript to_from_sexp.topscript.corrected)))))

(alias (
  (name code)
  (deps (sexp_option.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? sexp_option.topscript sexp_option.topscript.corrected)))))

(alias (
  (name code)
  (deps (sexp_opaque.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? sexp_opaque.topscript sexp_opaque.topscript.corrected)))))

(alias (
  (name code)
  (deps (print_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? print_sexp.topscript print_sexp.topscript.corrected)))))

(alias (
  (name code)
  (deps (manually_making_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff?
     manually_making_sexp.topscript
     manually_making_sexp.topscript.corrected)))))

(alias ((name sexp) (deps (list_top_packages.sh.sexp))))

(rule (
  (targets (list_top_packages.sh.sexp))
  (deps    (list_top_packages.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias (
  (name code)
  (deps (
    example_load.topscript example.scm example_broken.scm comment_heavy.scm))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? example_load.topscript example_load.topscript.corrected)))))

(alias (
  (name code)
  (deps (auto_making_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? auto_making_sexp.topscript auto_making_sexp.topscript.corrected)))))

(alias (
  (name code)
  (deps (sexp_list.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? sexp_list.topscript sexp_list.topscript.corrected)))))

(alias (
  (name code)
  (deps (sexp_printer.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? sexp_printer.topscript sexp_printer.topscript.corrected)))))

