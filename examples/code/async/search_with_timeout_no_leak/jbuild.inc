(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    run_search_with_timeout_no_leak.sh
    search_with_timeout_no_leak.ml
    jbuild
    jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_search_with_timeout_no_leak.sh)
    (diff?
     run_search_with_timeout_no_leak.sh
     run_search_with_timeout_no_leak.sh.corrected)))))

