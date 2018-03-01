(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    build_basic_md5_with_flags.sh basic_md5_with_flags.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_basic_md5_with_flags.sh)
    (diff?
     build_basic_md5_with_flags.sh
     build_basic_md5_with_flags.sh.corrected)))))

(alias (
  (name cram)
  (deps (
    run_basic_md5_flags_help.sh
    build_basic_md5_with_flags.sh
    jbuild
    jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_basic_md5_flags_help.sh)
    (diff? run_basic_md5_flags_help.sh run_basic_md5_flags_help.sh.corrected)))))

(alias (
  (name cram)
  (deps (md5_completion.sh build_basic_md5_with_flags.sh jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe md5_completion.sh)
    (diff? md5_completion.sh md5_completion.sh.corrected)))))

