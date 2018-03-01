(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    build_read_foo.errsh foo_broken_example.scm jbuild jbuild.inc read_foo.ml))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_read_foo.errsh)
    (diff? build_read_foo.errsh build_read_foo.errsh.corrected)))))

