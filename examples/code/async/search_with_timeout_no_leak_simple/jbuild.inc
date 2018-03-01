(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_search_with_timeout_no_leak_simple.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_search_with_timeout_no_leak_simple.sh)
    (diff?
     run_search_with_timeout_no_leak_simple.sh
     run_search_with_timeout_no_leak_simple.sh.corrected)))))

