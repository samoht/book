(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_read_json.sh book.json read_json.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_read_json.sh)
    (diff? run_read_json.sh run_read_json.sh.corrected)))))

