(jbuild_version 1)

(alias (
  (name cram)
  (deps (show_barrier_bench_help.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe show_barrier_bench_help.sh)
    (diff? show_barrier_bench_help.sh show_barrier_bench_help.sh.corrected)))))

(alias (
  (name cram)
  (deps (run_barrier_bench.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_barrier_bench.sh)
    (diff? run_barrier_bench.sh run_barrier_bench.sh.corrected)))))

