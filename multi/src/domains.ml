(* Abstract domains which can be used for the analyses. *)
let available_domains : (module Relational.Domain) list =
  [


(* Abstract domain used for the analyses. *)
    (module (NonRelational.MakeRelational (Dummy)));
    (module (NonRelational.MakeRelational (Parity)));
     
    (* (module Sos.Dom); *)
   (* (module (ReducedProd.Make (Quadratic.Dom) (NonRelational.MakeRelational (Intervals)))); *)
   (* (module Soscont.Dom); *)
   (* (module Quadratic.Dom); *)
   (* (module GraphDomain.Dom); *)
   (module (NonRelational.MakeRelational (Intervals.Int)));
   (module (NonRelational.MakeRelational (Intervals.Real)));

   (module (NonRelational.MakeRelational (Kildall.Int)));
   (module (NonRelational.MakeRelational (Kildall.Real))) 

  (* To be able to use this last two domains, you have to install the Apron
   * library and change the flag "actually_use_apron" in myocamlbuild.ml. *)
  (* ; *)
  (* (module ApronWrapper.Polka); *)
  (* (module ApronWrapper.Oct) *)
  ]
