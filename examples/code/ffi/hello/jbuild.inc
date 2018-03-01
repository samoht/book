(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_hello.sh jbuild jbuild.inc hello.ml))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build_hello.sh)
    (diff? build_hello.sh        build_hello.sh.corrected)))))

