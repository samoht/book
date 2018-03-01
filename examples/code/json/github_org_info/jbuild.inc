(jbuild_version 1)

(alias (
  (name cram)
  (deps (generate_github_org_types.sh github_org_t.mli jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe generate_github_org_types.sh)
    (diff?
     generate_github_org_types.sh
     generate_github_org_types.sh.corrected)))))

(alias (
  (name cram)
  (deps (generate_github_org_json.sh github_org_j.mli jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe generate_github_org_json.sh)
    (diff? generate_github_org_json.sh generate_github_org_json.sh.corrected)))))

(alias (
  (name cram)
  (deps (run_github_org.sh build_github_org.sh jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe run_github_org.sh)
    (diff? run_github_org.sh run_github_org.sh.corrected)))))

(alias (
  (name cram)
  (deps (build_github_org.sh github_org_info.ml jbuild jbuild.inc))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_github_org.sh)
    (diff? build_github_org.sh build_github_org.sh.corrected)))))

