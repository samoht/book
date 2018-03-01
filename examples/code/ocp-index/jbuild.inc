(jbuild_version 1)

(alias (
  (name cram)
  (deps (index_ncurses.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe index_ncurses.sh)
    (diff? index_ncurses.sh index_ncurses.sh.corrected)))))

