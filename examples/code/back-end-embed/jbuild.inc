(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_embed.sh embed_me1.ml embed_me2.ml))
  (action (
    progn
    (run   ${ROOT}/cram/cram.exe build_embed.sh)
    (diff? build_embed.sh        build_embed.sh.corrected)))))

(alias (
  (name cram)
  (deps (build_embed_c.sh embed_me1.ml embed_me2.ml))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_embed_c.sh)
    (diff? build_embed_c.sh build_embed_c.sh.corrected)))))

(alias (
  (name cram)
  (deps (run_debug_hello.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_debug_hello.sh)
    (diff? run_debug_hello.sh run_debug_hello.sh.corrected)))))

