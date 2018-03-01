(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    build_short_parser.sh
    short_parser.mly
    jbuild
    jbuild.inc
    json.ml
    lexer.mll
    prog.ml
    prog.mli))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_short_parser.sh)
    (diff? build_short_parser.sh build_short_parser.sh.corrected)))))

