(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    run_search_with_configurable_server.errsh
    search_with_configurable_server.ml
    jbuild
    jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_search_with_configurable_server.errsh)
    (diff?
     run_search_with_configurable_server.errsh
     run_search_with_configurable_server.errsh.corrected)))))

