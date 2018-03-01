(jbuild_version 1)

(alias (
  (name cram)
  (deps (build.errsh counter.ml counter.mli freq.ml jbuild jbuild.inc))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build.errsh)
    (diff? build.errsh           build.errsh.corrected)))))

