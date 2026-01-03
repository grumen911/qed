BeginPackage["QED`Interactive`", {"QED`Model`"}];

QubitDashboard::usage = "QubitDashboard[{models..}] - интерактивная панель управления для списка моделей.";

(* PUBLIC API для расширения графиков *)
RegisterPlot::usage = "RegisterPlot[id, label, type, computeFunc] регистрирует новый тип графика.";

Begin["`Private`"];

(* ═══════════════════════════════════════════════════════════════ *)
(* 1. BACKEND: PLOT REGISTRY & COMPUTE SYSTEM *)
(* ═══════════════════════════════════════════════════════════════ *)

(* Хранилище метаданных графиков *)
$PlotRegistry = <||>;

RegisterPlot[id_String, label_String, type_String, computeFunc_] := 
  ($PlotRegistry[id] = <|
    "Label" -> label, 
    "Type" -> type,       (* "Light" (auto) или "Heavy" (manual) *)
    "Compute" -> computeFunc
  |>);

(* Базовые графики регистрируем при загрузке пакета *)
RegisterPlot["PlasmonSpectrum", "Plasmon Spectrum", "Light", 
  Function[{m}, QED`Plots`PlotPlasmonSpectrum[m]]
];

RegisterPlot["Potential3D", "Potential Landscape 3D", "Heavy", 
  Function[{m}, QED`Plots`PlotPotentialSlices3D[m]]
];

(* Безопасная функция вычисления *)
ComputePlotData[plotId_, model_] := 
  Module[{info, func},
    info = $PlotRegistry[plotId];
    If[MissingQ[info], Return[Graphics[{Red, Text["Unknown Plot ID"]}]]];
    
    func = info["Compute"];
    (* Выполняем вычисление *)
    Check[func[model], Graphics[{Red, Text["Computation Failed"]}]]
  ];

(* ═══════════════════════════════════════════════════════════════ *)
(* 2. CORE: DASHBOARD SHELL (SKELETON) *)
(* ═══════════════════════════════════════════════════════════════ *)

QubitDashboard[modelsStack : {__Association}] := DynamicModule[
  {
    (* State *)
    currentModel = First[modelsStack],
    selectedPlotId = "PlasmonSpectrum",
    
    (* Cache (Transient) *)
    plotCache
  },
  
  (* VIEW *)
  Column[{
    (* Header: Model Info *)
    Dynamic @ Style["Model: " <> ToString[currentModel["Topology"]["Type"]], Bold, 16],
    
    (* Debug: Cache Status *)
    Dynamic @ Row[{"Cache Keys: ", Keys[plotCache]}],
    
    (* Content Placeholder *)
    Dynamic @ Panel[
      Column[{
        "Selected Plot: " <> selectedPlotId,
        ActionMenu["Choose Plot", 
          KeyValueMap[#1 :> (selectedPlotId = #1) &, $PlotRegistry]
        ]
      }]
    ]
  }],
  
  (* CONFIGURATION *)
  (* 1. Cache is transient, never saved to file *)
  UnsavedVariables :> {plotCache},
  
  (* 2. Clean initialization on every kernel start *)
  Initialization :> {
    plotCache = <||>;
  },
  
  (* 3. Do not block UI loading *)
  SynchronousInitialization -> False,
  
  (* 4. Rely on package definitions, do not embed functions *)
  SaveDefinitions -> False
];

End[];
EndPackage[];