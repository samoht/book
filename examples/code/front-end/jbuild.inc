(jbuild_version 1)

(alias (
  (name cram)
  (deps (infer_typedef.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe infer_typedef.sh)
    (diff? infer_typedef.sh infer_typedef.sh.corrected)))))

(alias (
  (name cram)
  (deps (build_principal_corebuild.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_principal_corebuild.sh)
    (diff?
     build_principal_corebuild.sh
     build_principal_corebuild.sh.corrected)))))

(alias (
  (name cram)
  (deps (build_broken_poly_with_annot.errsh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_broken_poly_with_annot.errsh)
    (diff?
     build_broken_poly_with_annot.errsh
     build_broken_poly_with_annot.errsh.corrected)))))

(alias (
  (name cram)
  (deps (indent_follow_on_function.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe indent_follow_on_function.sh)
    (diff?
     indent_follow_on_function.sh
     indent_follow_on_function.sh.corrected)))))

(alias (
  (name cram)
  (deps (typedtree_typedef.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe typedtree_typedef.sh)
    (diff? typedtree_typedef.sh typedtree_typedef.sh.corrected)))))

(alias (
  (name cram)
  (deps (process_comparelib_interface.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe process_comparelib_interface.sh)
    (diff?
     process_comparelib_interface.sh
     process_comparelib_interface.sh.corrected)))))

(alias (
  (name cram)
  (deps (build_type_conv_without_camlp4.errsh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_type_conv_without_camlp4.errsh)
    (diff?
     build_type_conv_without_camlp4.errsh
     build_type_conv_without_camlp4.errsh.corrected)))))

(alias (
  (name cram)
  (deps (build_principal.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_principal.sh)
    (diff? build_principal.sh build_principal.sh.corrected)))))

(alias (
  (name cram)
  (deps (conflicting_interfaces.errsh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe conflicting_interfaces.errsh)
    (diff?
     conflicting_interfaces.errsh
     conflicting_interfaces.errsh.corrected)))))

(alias (
  (name cram)
  (deps (build_broken_module.errsh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_broken_module.errsh)
    (diff? build_broken_module.errsh build_broken_module.errsh.corrected)))))

(alias (
  (name cram)
  (deps (indent_follow_on_function_fixed.sh follow_on_function_fixed.ml))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe indent_follow_on_function_fixed.sh)
    (diff?
     indent_follow_on_function_fixed.sh
     indent_follow_on_function_fixed.sh.corrected)))))

(alias (
  (name cram)
  (deps (parsetree_typedef.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe parsetree_typedef.sh)
    (diff? parsetree_typedef.sh parsetree_typedef.sh.corrected)))))

(alias (
  (name cram)
  (deps (typedef_objinfo.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe typedef_objinfo.sh)
    (diff? typedef_objinfo.sh typedef_objinfo.sh.corrected)))))

(alias (
  (name cram)
  (deps (build_non_principal.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_non_principal.sh)
    (diff? build_non_principal.sh build_non_principal.sh.corrected)))))

(alias (
  (name code)
  (deps (camlp4_toplevel.topscript))
  (action (
    progn
    (setenv OCAMLRUNPARAM "" (
      run ocaml-topexpect -short-paths -verbose camlp4_toplevel.topscript))
    (diff? camlp4_toplevel.topscript camlp4_toplevel.topscript.corrected)))))

(alias (
  (name cram)
  (deps (build_follow_on_function.errsh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_follow_on_function.errsh)
    (diff?
     build_follow_on_function.errsh
     build_follow_on_function.errsh.corrected)))))

(alias (
  (name cram)
  (deps (build_broken_poly.errsh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe build_broken_poly.errsh)
    (diff? build_broken_poly.errsh build_broken_poly.errsh.corrected)))))

(alias (
  (name cram)
  (deps (process_comparelib_test.sh))
  (action (
    progn
    (run ${ROOT}/cram/cram.exe process_comparelib_test.sh)
    (diff? process_comparelib_test.sh process_comparelib_test.sh.corrected)))))

