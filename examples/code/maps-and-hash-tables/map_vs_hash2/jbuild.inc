(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_map_vs_hash2.sh map_vs_hash2.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_map_vs_hash2.sh)
    (diff? run_map_vs_hash2.sh run_map_vs_hash2.sh.corrected)))))

