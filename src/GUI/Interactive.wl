BeginPackage["QED`Interactive`"];

QubitDashboard::usage = "QubitDashboard[model] - интерактивная панель управления";

Begin["`Private`"];

(* ═══════════════════════════════════════════════════════════════ *)
(* ЛОГИКА *)
(* ═══════════════════════════════════════════════════════════════ *)

ExtractInteractiveParams[model_Association] := 
  If[KeyExistsQ[model, "Primary"],
    Cases[
      Normal[model["Primary"]],
      (sym_ -> assoc_Association) /; 
        (TrueQ[assoc["Interactive"]] && KeyExistsQ[assoc, "Value"]) :> 
        {
          sym, 
          assoc["Value"],
          Lookup[assoc, "Min", 0],                    (* Значение по умолчанию 0 *)
          Lookup[assoc, "Max", 10],                   (* Значение по умолчанию 10 *)
          Lookup[assoc, "Step", 0.1],                 (* Значение по умолчанию 0.1 *)
          Lookup[assoc, "Label", ToString[sym]]       (* Значение по умолчанию - имя символа *)
        },
      Infinity
    ],
    {}
  ];

(*MakeDynamicSliderValue[sym_, currentValue_] :=
  Dynamic[
    If[KeyExistsQ[state, sym], state[[sym]], currentValue],
    (state[[sym]] = #) &
  ];*)
(* ═══════════════════════════════════════════════════════════════ *)
(* ОФОРМЛЕНИЕ *)
(* ═══════════════════════════════════════════════════════════════ *)

QubitDashboard[ListModels_Association] := DynamicModule[
  {modelIndex, model},
  		1
];

End[];
EndPackage[];