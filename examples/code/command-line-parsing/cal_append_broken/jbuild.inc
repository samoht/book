(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_cal_append_broken.errsh cal_append_broken.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_cal_append_broken.errsh)
    (diff?
     build_cal_append_broken.errsh
     build_cal_append_broken.errsh.corrected)))))

