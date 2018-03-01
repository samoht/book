(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_alternate_list.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_alternate_list.sh)
    (diff? run_alternate_list.sh run_alternate_list.sh.corrected)))))

