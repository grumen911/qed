ClearAll["QED`Interactive`*"]
BeginPackage["QED`Interactive`"];


QubitDashboard::usage = "QubitDashboard[model ] - интерактивная панель управления";


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
          {Lookup[assoc, "Min", 0],                    (* Значение по умолчанию 0 *)
          Lookup[assoc, "Max", 10],                   (* Значение по умолчанию 10 *)
          Lookup[assoc, "Step", 0.1]}                 (* Значение по умолчанию 0.1 *)
          (*Lookup[assoc, "Label", ToString[sym]]*)       (* Значение по умолчанию - имя символа *)
        },
      Infinity
    ],
    {}
  ];


(* ═══════════════════════════════════════════════════════════════ *)
(* ОФОРМЛЕНИЕ *)
(* ═══════════════════════════════════════════════════════════════ *)

	(*Создать слайдер*)	
	(*MakeDynamicSlider[model_, sym_String] :=
  	MakeDynamicSlider[model, sym, 5, {0.001, 10, 0.01}];*)
  	
	MakeDynamicSlider[model_, sym_, currentValue_, {min_, max_, step_}] :=
	  With[{
	    d = model["Primary",sym,"Value"](*Dynamic[model["Primary"][sym]["Value"], 
	                (model["Primary"][sym]["Value"] = #) &]*)
	  },
	    Row[{
	      sym <> ": ",
	      Slider[d, {min, max, step}],
	      InputField[d, Number, FieldSize -> {5, 1}]
	    }]
	  ];
		
		
	MakeSliderHub[model_] := Module[{params},
		params = ExtractInteractiveParams[model];
		  Column[{
		    Map[
		      Function[{paramList},
		        MakeDynamicSlider[model, Sequence @@ paramList]
		      ],
		      params
		    ]
		  }]
	];


  	(*Кнопка выбора работы*)  
SelectModel[model_, modelsStack_] :=
  Row[{
    Pane[
      SetterBar[
        Dynamic[model],
        	  MapThread[#2 -> #1 &,
		    {Query[All, "Primary", "Image"][modelsStack],
		    	 modelsStack}
        ],
        Appearance -> "Vertical"
      ],
      ImageSize -> {All, 200},
      Scrollbars -> {False, True}
    ],
    
    Dynamic[
      Graphics[
        model["Primary", "Image"],
        ImageSize -> {All, 200}
      ],
      TrackedSymbols :> {model}
    ]
  }];



QubitDashboard[modelsStack : {Association__}] := DynamicModule[
  {model = First@modelsStack},
  
  Column[{
   SelectModel[Unevaluated@model, modelsStack],
    Dynamic[model]
  }]
];


End[];
EndPackage[];