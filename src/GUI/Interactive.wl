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
      Function[{tag, params},
        KeyValueMap[
          {tag, #1, #2["Value"], {#2["Min"], #2["Max"], #2["Step"]}} &,
          params
        ]
      ],
      Query[All, Select[#Interactive === True &]][model["Primary"]]
    ],
    1
  ];


(* ═══════════════════════════════════════════════════════════════ *)
(* ОФОРМЛЕНИЕ *)
(* ═══════════════════════════════════════════════════════════════ *)


(*Создать слайдер*)	
MakeDynamicSlider[model_, sym_, currentValue_, {min_, max_, step_}] :=
  With[{
    d = Dynamic[model["Primary"][sym]["Value"], 
                (model["Primary"][sym]["Value"] = #;
                 $CurrentModel = model) &]
  },
    Row[{
      sym <> ": ",
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
   MakeDynamicSlider[Unevaluated@model, "eJ", 2, {0, 5, 0.01}],
   MakeSliderHub[Unevaluated@model],
    Dynamic[model]
  }]
];


End[];
EndPackage[];