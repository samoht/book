(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_map_vs_hash.sh map_vs_hash.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_map_vs_hash.sh)
    (diff? run_map_vs_hash.sh run_map_vs_hash.sh.corrected)))))

