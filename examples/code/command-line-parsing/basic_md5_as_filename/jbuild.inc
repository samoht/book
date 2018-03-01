(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    run_basic_md5_as_filename.errsh
    basic_md5_as_filename.ml
    jbuild
    jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_basic_md5_as_filename.errsh)
    (diff?
     run_basic_md5_as_filename.errsh
     run_basic_md5_as_filename.errsh.corrected)))))

