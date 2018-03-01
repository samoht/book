(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_bench_patterns.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_bench_patterns.sh)
    (diff? run_bench_patterns.sh run_bench_patterns.sh.corrected)))))

