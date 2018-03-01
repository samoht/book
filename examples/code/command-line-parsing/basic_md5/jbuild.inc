(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_basic_md5.sh build_basic_md5.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_basic_md5.sh)
    (diff? run_basic_md5.sh run_basic_md5.sh.corrected)))))

(alias (
  (name cram)
  (deps (build_basic_md5.sh basic_md5.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_basic_md5.sh)
    (diff? build_basic_md5.sh build_basic_md5.sh.corrected)))))

(alias (
  (name cram)
  (deps (get_basic_md5_help.errsh build_basic_md5.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe get_basic_md5_help.errsh)
    (diff? get_basic_md5_help.errsh get_basic_md5_help.errsh.corrected)))))

(alias (
  (name cram)
  (deps (get_basic_md5_version.sh build_basic_md5.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe get_basic_md5_version.sh)
    (diff? get_basic_md5_version.sh get_basic_md5_version.sh.corrected)))))

