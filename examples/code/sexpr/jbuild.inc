(jbuild_version 1)

(alias (
  (name code)
  (deps (inline_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose inline_sexp.topscript))
    (diff? inline_sexp.topscript inline_sexp.topscript.corrected)))))

(alias (
  (name code)
  (deps (sexp_default.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose sexp_default.topscript))
    (diff? sexp_default.topscript sexp_default.topscript.corrected)))))

(alias (
  (name code)
  (deps (to_from_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose to_from_sexp.topscript))
    (diff? to_from_sexp.topscript to_from_sexp.topscript.corrected)))))

(alias (
  (name code)
  (deps (sexp_option.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose sexp_option.topscript))
    (diff? sexp_option.topscript sexp_option.topscript.corrected)))))

(alias (
  (name code)
  (deps (sexp_opaque.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose sexp_opaque.topscript))
    (diff? sexp_opaque.topscript sexp_opaque.topscript.corrected)))))

(alias (
  (name code)
  (deps (print_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose print_sexp.topscript))
    (diff? print_sexp.topscript print_sexp.topscript.corrected)))))

(alias (
  (name code)
  (deps (manually_making_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run
      ocaml-topexpect
      -short-paths
      -verbose
      manually_making_sexp.topscript))
    (diff?
     manually_making_sexp.topscript
     manually_making_sexp.topscript.corrected)))))

(alias (
  (name cram)
  (deps (list_top_packages.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe list_top_packages.sh)
    (diff? list_top_packages.sh list_top_packages.sh.corrected)))))

(alias (
  (name code)
  (deps (
    example_load.topscript example.scm example_broken.scm comment_heavy.scm))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose example_load.topscript))
    (diff? example_load.topscript example_load.topscript.corrected)))))

(alias (
  (name code)
  (deps (auto_making_sexp.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose auto_making_sexp.topscript))
    (diff? auto_making_sexp.topscript auto_making_sexp.topscript.corrected)))))

(alias (
  (name code)
  (deps (sexp_list.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose sexp_list.topscript))
    (diff? sexp_list.topscript sexp_list.topscript.corrected)))))

(alias (
  (name code)
  (deps (sexp_printer.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose sexp_printer.topscript))
    (diff? sexp_printer.topscript sexp_printer.topscript.corrected)))))

