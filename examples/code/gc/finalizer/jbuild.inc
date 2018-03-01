(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_finalizer.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_finalizer.sh)
    (diff? run_finalizer.sh run_finalizer.sh.corrected)))))

