(lang dune 3.6)

(name piece_rope)

(generate_opam_files true)

(source
 (github hummy123/ocaml-piecerope))

(authors "Humza Shahid")

(maintainers "Humza Shahid")

(license  MIT)

(package
 (name piece_rope)
 (synopsis "A data structure for efficiently manipulating strings")
 (description "Supports UTF-8, UTF-16 and UTF-32 for interacting with external systems. Provides extensibility points for you to implement domain-specific functions. Documented and designed to be simple to use.")
 (depends 
   (ocaml (>= 4.13.0)) 
   dune 
   (atdgen (>= 2.11.0)) 
   (yojson (>= 2.0.2))
   (alcotest (>= 1.7.0))
   (bisect_ppx (>= 2.5.0)))
 (tags
  (text "piece table" "piece tree" rope unicode)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
