(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    build_query_handler_loader.sh
    query_handler.ml
    query_handler_core.ml
    query_handler_loader.ml
    jbuild
    jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_query_handler_loader.sh)
    (diff?
     build_query_handler_loader.sh
     build_query_handler_loader.sh.corrected)))))

