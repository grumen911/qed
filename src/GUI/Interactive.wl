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
  	(*Кнопка выбора работы*)  
	 SelectModel[modelKey_ ,model_, modelsStack_] := Module[{frontend, dynamicFig, dynamicSys},
	 	frontend[v1_,v2_] := 
		Row[{ Pane[SetterBar[v1, 
			  Keys[modelsStack], 
			  Appearance -> "Vertical"], ImageSize -> {All, 200}, 
			 Scrollbars -> {False, True}], v2}];
		  
		dynamicFig :=Dynamic[modelKey,
				      (modelKey = #;
				       model = modelsStack[#];
				       init[modelsStack[#]]) &
    		];
		dynamicSys := Dynamic[
			Graphics[modelKey, 
			ImageSize -> {All, 200}],
			TrackedSymbols:>{modelKey},
			SynchronousUpdating -> False
		];
		
		frontend[dynamicFig,dynamicSys]
	 ];



QubitDashboard[modelsStack : _Association] := DynamicModule[
  {modelKey, model},
  
  {modelKey, model} = First@Normal[modelsStack];
  	
  	{SelectModel[modelKey, model, modelsStack],Dynamic[model]}
];


End[];
EndPackage[];