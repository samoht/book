(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_qsort.sh input.txt jbuild jbuild.inc qsort.ml))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build_qsort.sh)
    (diff? build_qsort.sh        build_qsort.sh.corrected)))))

