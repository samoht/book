(jbuild_version 1)

(alias (
  (name code)
  (deps (install.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose install.topscript))
    (diff? install.topscript install.topscript.corrected)))))

(alias (
  (name code)
  (deps (parse_book.topscript book.json))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose parse_book.topscript))
    (diff? parse_book.topscript parse_book.topscript.corrected)))))

(alias (
  (name code)
  (deps (build_json.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose build_json.topscript))
    (diff? build_json.topscript build_json.topscript.corrected)))))

(alias (
  (name cram)
  (deps (build_github_atd.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_github_atd.sh)
    (diff? build_github_atd.sh build_github_atd.sh.corrected)))))

