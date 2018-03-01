(jbuild_version 1)

(alias (
  (name cram)
  (deps (instr_for_pattern_monomorphic_small.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe instr_for_pattern_monomorphic_small.sh)
    (diff?
     instr_for_pattern_monomorphic_small.sh
     instr_for_pattern_monomorphic_small.sh.corrected)))))

(alias (
  (name cram)
  (deps (asm_from_compare_mono.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe asm_from_compare_mono.sh)
    (diff? asm_from_compare_mono.sh asm_from_compare_mono.sh.corrected)))))

(alias (
  (name cram)
  (deps (lambda_for_pattern_monomorphic_large.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe lambda_for_pattern_monomorphic_large.sh)
    (diff?
     lambda_for_pattern_monomorphic_large.sh
     lambda_for_pattern_monomorphic_large.sh.corrected)))))

(alias (
  (name cram)
  (deps (lambda_for_pattern_monomorphic_small.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe lambda_for_pattern_monomorphic_small.sh)
    (diff?
     lambda_for_pattern_monomorphic_small.sh
     lambda_for_pattern_monomorphic_small.sh.corrected)))))

(alias (
  (name cram)
  (deps (lambda_for_pattern_polymorphic.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe lambda_for_pattern_polymorphic.sh)
    (diff?
     lambda_for_pattern_polymorphic.sh
     lambda_for_pattern_polymorphic.sh.corrected)))))

