(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    run_search_with_error_handling.sh
    search_with_error_handling.ml
    jbuild
    jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_search_with_error_handling.sh)
    (diff?
     run_search_with_error_handling.sh
     run_search_with_error_handling.sh.corrected)))))

