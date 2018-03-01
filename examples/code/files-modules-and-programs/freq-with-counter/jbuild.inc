(jbuild_version 1)

(alias (
  (name cram)
  (deps (build.sh freq.ml counter.ml jbuild jbuild.inc))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build.sh)
    (diff? build.sh              build.sh.corrected)))))

(alias (
  (name cram)
  (deps (infer_mli.sh))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe infer_mli.sh)
    (diff? infer_mli.sh          infer_mli.sh.corrected)))))

