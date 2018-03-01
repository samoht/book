(jbuild_version 1)

(alias (
  (name cram)
  (deps (build.errsh))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build.errsh)
    (diff? build.errsh           build.errsh.corrected)))))

