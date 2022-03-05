(lang dune 3.0)

(name qcheck_file)

(generate_opam_files true)

(source
 (github liyishuai/qcheck_file))

(authors "Yishuai Li")

(maintainers "Yishuai Li")

(license MPL-2.0)

(package
 (name qcheck_file)
 (synopsis "Testing file operation")
 (description "Property-based testing of OCaml file system API.")
 (depends ocaml dune qcheck))
