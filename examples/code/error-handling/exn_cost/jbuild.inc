(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_exn_cost.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_exn_cost.sh)
    (diff? run_exn_cost.sh run_exn_cost.sh.corrected)))))

(alias (
  (name cram)
  (deps (run_exn_cost_notrace.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_exn_cost_notrace.sh)
    (diff? run_exn_cost_notrace.sh run_exn_cost_notrace.sh.corrected)))))

