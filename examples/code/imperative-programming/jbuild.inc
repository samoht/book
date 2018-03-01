(jbuild_version 1)

(alias (
  (name code)
  (deps (examples.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? examples.topscript examples.topscript.corrected)))))

(alias (
  (name code)
  (deps (value_restriction.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? value_restriction.topscript value_restriction.topscript.corrected)))))

(alias (
  (name code)
  (deps (weak.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? weak.topscript weak.topscript.corrected)))))

(alias (
  (name code)
  (deps (file2.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? file2.topscript file2.topscript.corrected)))))

(alias (
  (name code)
  (deps (lazy.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? lazy.topscript lazy.topscript.corrected)))))

(alias (
  (name code)
  (deps (file.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? file.topscript file.topscript.corrected)))))

(alias (
  (name code)
  (deps (ref.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? ref.topscript ref.topscript.corrected)))))

(alias (
  (name code)
  (deps (printf.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? printf.topscript printf.topscript.corrected)))))

(alias (
  (name code)
  (deps (letrec.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? letrec.topscript letrec.topscript.corrected)))))

(alias (
  (name code)
  (deps (order.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? order.topscript order.topscript.corrected)))))

(alias (
  (name code)
  (deps (for.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? for.topscript for.topscript.corrected)))))

(alias (
  (name code)
  (deps (fib.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? fib.topscript fib.topscript.corrected)))))

(alias (
  (name code)
  (deps (memo.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (run ocaml-topexpect -short-paths -verbose ${<}))
    (diff? memo.topscript memo.topscript.corrected)))))

