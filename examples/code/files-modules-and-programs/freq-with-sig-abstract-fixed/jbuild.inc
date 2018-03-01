(jbuild_version 1)

(alias (
  (name cram)
  (deps (build.sh counter.ml counter.mli freq.ml jbuild jbuild.inc))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build.sh)
    (diff? build.sh              build.sh.corrected)))))

