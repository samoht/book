(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_parse_book.sh book.json parse_book.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_parse_book.sh)
    (diff? run_parse_book.sh run_parse_book.sh.corrected)))))

