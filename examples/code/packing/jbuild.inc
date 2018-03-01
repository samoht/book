(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_test.sh))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build_test.sh)
    (diff? build_test.sh         build_test.sh.corrected)))))

(alias (
  (name cram)
  (deps (show_files.sh))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe show_files.sh)
    (diff? show_files.sh         show_files.sh.corrected)))))

