#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ecaml"
  [ oasis_lib "ecaml"
  ; file "META" ~section:"lib"
  ]
