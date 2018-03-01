(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_shapes.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_shapes.sh)
    (diff? build_shapes.sh build_shapes.sh.corrected)))))

