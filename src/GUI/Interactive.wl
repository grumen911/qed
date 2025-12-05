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
          assoc["Min", 0],                    (* Значение по умолчанию 0 *)
          assoc["Max", 10],                   (* Значение по умолчанию 10 *)
          assoc["Step", 0.01],                 (* Значение по умолчанию 0.1 *)
          assoc["Label", ToString[sym]]       (* Значение по умолчанию - имя символа *)
        },
      Infinity
    ],
    {}
  ];

(* ═══════════════════════════════════════════════════════════════ *)
(* ОФОРМЛЕНИЕ *)
(* ═══════════════════════════════════════════════════════════════ *)

MakeParametersPanel[params_, state_] :=
  Column[
    Table[
      Row[{ToString[sym] <> ": ", Dynamic[state[sym]]}],
      {sym, First /@ params}
    ],
    Spacings -> 1
  ]; 
   

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