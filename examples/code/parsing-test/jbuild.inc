(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_broken_test.errsh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_broken_test.errsh)
    (diff? run_broken_test.errsh run_broken_test.errsh.corrected)))))

(alias (
  (name cram)
  (deps (build_test.sh))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build_test.sh)
    (diff? build_test.sh         build_test.sh.corrected)))))

