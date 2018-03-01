(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_blow_up.errsh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_blow_up.errsh)
    (diff? build_blow_up.errsh build_blow_up.errsh.corrected)))))

(alias (
  (name cram)
  (deps (build_blow_up_notrace.errsh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_blow_up_notrace.errsh)
    (diff? build_blow_up_notrace.errsh build_blow_up_notrace.errsh.corrected)))))

