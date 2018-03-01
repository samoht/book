(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    build_read_foo_better_errors.errsh
    jbuild
    jbuild.inc
    foo_broken_example.scm
    read_foo_better_errors.ml))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_read_foo_better_errors.errsh)
    (diff?
     build_read_foo_better_errors.errsh
     build_read_foo_better_errors.errsh.corrected)))))

