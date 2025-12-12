ClearAll["QED`Interactive`*"]
(*BeginPackage["QED`Interactive`", {"QED`Model`"}];*)
BeginPackage["QED`Interactive`"];

Needs["QED`Model`"];

QubitDashboard::usage = "QubitDashboard[model ] - интерактивная панель управления";


Begin["`Private`"];

(* ═══════════════════════════════════════════════════════════════ *)
(* ЛОГИКА *)
(* ═══════════════════════════════════════════════════════════════ *)

ExtractInteractiveParams[model_Association] :=
  Flatten[
    KeyValueMap[
      Function[{tag, componentParams},
        KeyValueMap[
          {tag, #1, #2["Value"], {#2["Min"], #2["Max"], #2["Step"]}} &,
          Select[componentParams, AssociationQ[#] && 
          	Lookup[#, "Interactive", False] === True &]
        ]
      ],
      model["Primary"]
    ],
    1
  ];


(* ═══════════════════════════════════════════════════════════════ *)
(* ОФОРМЛЕНИЕ *)
(* ═══════════════════════════════════════════════════════════════ *)


(*Создать слайдер*)	
MakeDynamicSlider[model_, componentTag_, paramName_, currentValue_, 
				 {min_, max_, step_}] :=
  With[{
    d = Dynamic[
      model["Primary"][componentTag][paramName]["Value"], 
      (model["Primary"][componentTag][paramName]["Value"] = #;
       $CurrentModel = model) &
    ]
  },
    Row[{
      componentTag <> "." <> paramName <> ": ",
      Slider[d, {min, max, step}],
      InputField[d, Number, FieldSize -> {5, 1}]
    }]
  ];
		
	
(*Создает массив слайдеров*)	
MakeSliderHub[model_] := Module[{params},
	params = ExtractInteractiveParams[model];
	  Column[
	    Map[
	      Function[{paramList},
	        MakeDynamicSlider[Unevaluated@model, Sequence @@ paramList]
	      ],
	      params
	    ]
	  ]
];


(*Кнопка выбора работы*)  
SelectModel[model_, modelsStack_] :=
  Row[{
    Pane[
      SetterBar[
        Dynamic[model, (model = #; $CurrentModel = #) &],
        	  MapThread[#2 -> #1 &,
		    {Query[All, "Image"][modelsStack],
		    	 modelsStack}
        ],
        Appearance -> "Vertical"
      ],
      ImageSize -> {All, 200},
      Scrollbars -> {False, True}
    ],
    
    Dynamic[
      Graphics[
        model["Image"],
        ImageSize -> {All, 200}
      ],
      TrackedSymbols :> {model}
    ]
  }];


QubitDashboard[modelsStack : {Association__}] := DynamicModule[
  {model = First@modelsStack},
  
  Column[{
   SelectModel[Unevaluated@model, modelsStack],
   MakeSliderHub[Unevaluated@model],
    Dynamic[model["Analytical","CapacitanceMatrix"]/.model["SubstitutionRules"]]
  }]
];


End[];
EndPackage[];