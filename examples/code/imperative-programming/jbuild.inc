(jbuild_version 1)

(alias (
  (name code)
  (deps (examples.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose examples.topscript))
    (diff? examples.topscript examples.topscript.corrected)))))

(alias (
  (name code)
  (deps (value_restriction.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose value_restriction.topscript))
    (diff? value_restriction.topscript value_restriction.topscript.corrected)))))

(alias (
  (name code)
  (deps (weak.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose weak.topscript))
    (diff? weak.topscript weak.topscript.corrected)))))

(alias (
  (name code)
  (deps (file2.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose file2.topscript))
    (diff? file2.topscript file2.topscript.corrected)))))

(alias (
  (name code)
  (deps (lazy.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose lazy.topscript))
    (diff? lazy.topscript lazy.topscript.corrected)))))

(alias (
  (name code)
  (deps (file.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose file.topscript))
    (diff? file.topscript file.topscript.corrected)))))

(alias (
  (name code)
  (deps (ref.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose ref.topscript))
    (diff? ref.topscript ref.topscript.corrected)))))

(alias (
  (name code)
  (deps (printf.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose printf.topscript))
    (diff? printf.topscript printf.topscript.corrected)))))

(alias (
  (name code)
  (deps (letrec.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose letrec.topscript))
    (diff? letrec.topscript letrec.topscript.corrected)))))

(alias (
  (name code)
  (deps (order.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose order.topscript))
    (diff? order.topscript order.topscript.corrected)))))

(alias (
  (name code)
  (deps (for.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose for.topscript))
    (diff? for.topscript for.topscript.corrected)))))

(alias (
  (name code)
  (deps (fib.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose fib.topscript))
    (diff? fib.topscript fib.topscript.corrected)))))

(alias (
  (name code)
  (deps (memo.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose memo.topscript))
    (diff? memo.topscript memo.topscript.corrected)))))

