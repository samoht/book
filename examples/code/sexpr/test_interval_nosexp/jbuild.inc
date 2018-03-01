(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_test_interval_nosexp.errsh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_test_interval_nosexp.errsh)
    (diff?
     build_test_interval_nosexp.errsh
     build_test_interval_nosexp.errsh.corrected)))))

