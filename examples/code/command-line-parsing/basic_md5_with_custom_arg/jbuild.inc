(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    run_basic_md5_with_custom_arg.errsh
    jbuild
    jbuild.inc
    basic_md5_with_custom_arg.ml))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_basic_md5_with_custom_arg.errsh)
    (diff?
     run_basic_md5_with_custom_arg.errsh
     run_basic_md5_with_custom_arg.errsh.corrected)))))

