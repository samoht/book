(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    build_and_run_cal_add_interactive.sh
    cal_add_interactive.ml
    jbuild
    jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_and_run_cal_add_interactive.sh)
    (diff?
     build_and_run_cal_add_interactive.sh
     build_and_run_cal_add_interactive.sh.corrected)))))

