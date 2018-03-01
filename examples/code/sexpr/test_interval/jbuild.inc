(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_test_interval.sh test_interval.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_test_interval.sh)
    (diff? build_test_interval.sh build_test_interval.sh.corrected)))))

