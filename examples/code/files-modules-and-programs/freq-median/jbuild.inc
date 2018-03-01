(jbuild_version 1)

(alias (
  (name cram)
  (deps (build.sh))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build.sh)
    (diff? build.sh              build.sh.corrected)))))

