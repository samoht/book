(jbuild_version 1)

(alias (
  (name cram)
  (deps (build.sh jbuild.inc jbuild freq.ml))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build.sh)
    (diff? build.sh              build.sh.corrected)))))

(alias (
  (name cram)
  (deps (test.sh build.sh))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe test.sh)
    (diff? test.sh               test.sh.corrected)))))

