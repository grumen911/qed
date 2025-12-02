BeginPackage["QED`GUI`"];

MyInteractiveModule::usage = "MyInteractiveModule[initParams] creates interactive UI.";
MyCompareModule::usage = "MyCompareModule[p1, p2] compares two parameter sets.";

Begin["`Private`"];

MyInteractiveModule[initParams_Association] := DynamicModule[
  {params = initParams, model, tmax = 10},
  Initialization :> (
    model = QED`Model`CreateModel[params];
  ),
  Column[{
    Row[{"Ω: ", Slider[Dynamic[params["Omega"], 1], 1], 11}],
    Dynamic[
      QED`Numeric`ComputeEvolution[model, tmax];
      QED`Style`QubitPlot[Re@model["Numeric"]["Evolution"][t], {t, 0, tmax}]
    ]
  }]
];

MyCompareModule[initParams1_, initParams2_] := DynamicModule[
  {params1 = initParams1, params2 = initParams2, model1, model2, tmax = 10},
  Initialization :> (
    model1 = QED`Model`CreateModel[params1];
    model2 = QED`Model`CreateModel[params2];
  ),
  Column[{
    (* слайдеры для params1 и params2 *)
    Dynamic[
      Plot[{Re@1}, {t, 0, tmax}]
    ]
  }]
];

End[];
EndPackage[];