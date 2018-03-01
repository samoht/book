(jbuild_version 1)

(alias (
  (name cram)
  (deps (simple_build.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe simple_build.sh)
    (diff? simple_build.sh simple_build.sh.corrected)))))

(alias (
  (name cram)
  (deps (simple_build_fail.errsh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe simple_build_fail.errsh)
    (diff? simple_build_fail.errsh simple_build_fail.errsh.corrected)))))

