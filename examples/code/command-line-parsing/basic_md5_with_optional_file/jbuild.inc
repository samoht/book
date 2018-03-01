(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    build_basic_and_default_md5.sh
    basic_md5_with_default_file.ml
    jbuild
    jbuild.inc
    basic_md5_with_optional_file.ml))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_basic_and_default_md5.sh)
    (diff?
     build_basic_and_default_md5.sh
     build_basic_and_default_md5.sh.corrected)))))

(alias (
  (name cram)
  (deps (run_basic_and_default_md5.sh build_basic_and_default_md5.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_basic_and_default_md5.sh)
    (diff?
     run_basic_and_default_md5.sh
     run_basic_and_default_md5.sh.corrected)))))

