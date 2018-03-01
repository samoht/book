(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_bench_poly_and_mono.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_bench_poly_and_mono.sh)
    (diff? run_bench_poly_and_mono.sh run_bench_poly_and_mono.sh.corrected)))))

