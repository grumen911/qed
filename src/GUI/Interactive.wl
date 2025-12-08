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


(* ═══════════════════════════════════════════════════════════════ *)
(* ОФОРМЛЕНИЕ *)
(* ═══════════════════════════════════════════════════════════════ *)

	(*Создать слайдер*)	
	MakeDynamicSlider[model_, sym_] :=
  	MakeDynamicSlider[model, sym, 5, {0.001, 10, 0.01}];
  	
	MakeDynamicSlider[model_ ,sym_, currentValue_, {min_, max_, step_}] :=
		With[
			{d = Dynamic[
					    model["Primary"][sym]["Value"],
					    (model["Primary"][sym]["Value"] = #) &,
					    TrackedSymbols :> {model}
				 ]
			},
			Row[{
				sym <> ": ",	
				Slider[
					d,
				    {min, max, step}],
			    InputField[d, Number, FieldSize -> {5, 1}]
			 }]
		];


  	(*Кнопка выбора работы*)  
	 SelectModel[modelKey_ ,model_, modelsStack_] := Module[{frontend, dynamicFig, dynamicSys},
	 	frontend[v1_,v2_] := 
		Row[{ Pane[SetterBar[v1, 
			  Keys[modelsStack], 
			  Appearance -> "Vertical"], ImageSize -> {All, 200}, 
			 Scrollbars -> {False, True}], v2}];
		  
		dynamicFig :=Dynamic[modelKey,
				      (modelKey = #;
				       model = modelsStack[modelKey];
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
  	
  	{SelectModel[modelKey, model, modelsStack],
  		MakeDynamicSlider[model, "eJ"],
  		Dynamic[model]}
];


End[];
EndPackage[];