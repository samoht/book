(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_echo.sh jbuild jbuild.inc echo.ml))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe run_echo.sh)
    (diff? run_echo.sh           run_echo.sh.corrected)))))

