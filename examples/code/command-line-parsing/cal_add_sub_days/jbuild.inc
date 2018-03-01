(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_cal_add_sub_days.sh cal_add_sub_days.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_cal_add_sub_days.sh)
    (diff? build_cal_add_sub_days.sh build_cal_add_sub_days.sh.corrected)))))

(alias (
  (name cram)
  (deps (run_cal_add_sub_days.sh build_cal_add_sub_days.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_cal_add_sub_days.sh)
    (diff? run_cal_add_sub_days.sh run_cal_add_sub_days.sh.corrected)))))

