(jbuild_version 1)

(alias (
  (name cram)
  (deps (infer_ncurses.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe infer_ncurses.sh)
    (diff? infer_ncurses.sh infer_ncurses.sh.corrected)))))

