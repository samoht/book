(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_session_info.errsh session_info.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_session_info.errsh)
    (diff? build_session_info.errsh build_session_info.errsh.corrected)))))

