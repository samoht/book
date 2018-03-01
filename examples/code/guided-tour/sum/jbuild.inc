(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_sum.sh jbuild jbuild.inc sum.ml))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build_sum.sh)
    (diff? build_sum.sh          build_sum.sh.corrected)))))

