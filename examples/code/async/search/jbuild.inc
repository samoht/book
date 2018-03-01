(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_search.sh search.ml jbuild jbuild.inc))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe run_search.sh)
    (diff? run_search.sh         run_search.sh.corrected)))))

