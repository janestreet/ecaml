module Caml_embed = Caml_embed
module Ecaml_callback = Ecaml_callback
module Feature = Feature
module Form = Form
module Funcall = Funcall
module Function = Function
module Generated_bindings = Generated_bindings
module Symbol = Symbol
module Value = Value
module Valueable = Valueable

let message = Value.message
let messagef = Value.messagef
let message_s = Value.message_s

let initialize () =
  Caml_embed.initialize;
  Ecaml_callback.initialize_module;
  Value.initialize_module
;;
