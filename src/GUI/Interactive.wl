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
          Lookup[assoc, "Min", 0],                    (* Значение по умолчанию 0 *)
          Lookup[assoc, "Max", 10],                   (* Значение по умолчанию 10 *)
          Lookup[assoc, "Step", 0.1],                 (* Значение по умолчанию 0.1 *)
          Lookup[assoc, "Label", ToString[sym]]       (* Значение по умолчанию - имя символа *)
        },
      Infinity
    ],
    {}
  ];

MakeDynamicSliderValue[sym_, currentValue_, state_] :=
  Dynamic[
    If[KeyExistsQ[state, sym], state[[sym]], currentValue],
    (state[[sym]] = #) &
  ];
(* ═══════════════════════════════════════════════════════════════ *)
(* ОФОРМЛЕНИЕ *)
(* ═══════════════════════════════════════════════════════════════ *)

RenderSliderControl[sym_, currentValue_, minVal_, maxVal_, stepVal_, label_, state_] :=
  Row[{
    Style[label, Bold, 12],
    " = ",
    Slider[
      MakeDynamicSliderValue[sym, currentValue, state],
      {minVal, maxVal, stepVal}, 
      Appearance -> "Labeled"
    ],
    " ",
    Dynamic[NumberForm[
      If[KeyExistsQ[state, sym], state[[sym]], currentValue],
      4
    ]]
  }];
   

QubitDashboard[models:{_Association..}] := DynamicModule[
  {
    (* ═══ ОБЩИЕ ПЕРЕМЕННЫЕ ═══ *)
    selectedModelIndex = 1,        (* Какая модель выбрана *)
    currentModel,                   (* Текущая модель *)
    
    (* ═══ ПЕРЕМЕННЫЕ МОДЕЛИ ═══ *)
    modelVariables = <||>           (* Параметры конкретной модели *)
  },
  
  Column[{
    (* Содержимое будет здесь *)
    Text["Dashboard placeholder"]
  }]
];

End[];
EndPackage[];