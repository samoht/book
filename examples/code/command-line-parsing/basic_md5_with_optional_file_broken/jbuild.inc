(jbuild_version 1)

(alias (
  (name cram)
  (deps (
    build_basic_md5_with_optional_file_broken.errsh
    basic_md5_with_optional_file_broken.ml
    jbuild
    jbuild.inc))
  (action (
    progn
    (run
     ${ROOT}/cram/cram.exe
     build_basic_md5_with_optional_file_broken.errsh)
    (diff?
     build_basic_md5_with_optional_file_broken.errsh
     build_basic_md5_with_optional_file_broken.errsh.corrected)))))

