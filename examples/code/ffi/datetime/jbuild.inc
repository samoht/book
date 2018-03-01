(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_datetime.sh datetime.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_datetime.sh)
    (diff? build_datetime.sh build_datetime.sh.corrected)))))

