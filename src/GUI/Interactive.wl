BeginPackage["QED`Interactive`", {"QED`Model`"}];

QubitDashboard::usage = "QubitDashboard[{models..}] - интерактивная панель управления для списка моделей.";
RegisterPlot::usage = "RegisterPlot[id, label, type, computeFunc] регистрирует новый тип графика.";

Begin["`Private`"];

(* ═══════════════════════════════════════════════════════════════ *)
(* 1. BACKEND: PLOT REGISTRY & COMPUTE SYSTEM *)
(* ═══════════════════════════════════════════════════════════════ *)

$PlotRegistry = <||>;

RegisterPlot[id_String, label_String, type_String, computeFunc_] := 
  ($PlotRegistry[id] = <|
    "Label" -> label, 
    "Type" -> type,
    "Compute" -> computeFunc
  |>);

(* Регистрация базовых графиков *)
RegisterPlot["PlasmonSpectrum", "Plasmon Spectrum", "Light", 
  Function[{m}, QED`Plots`PlotPlasmonSpectrum[m]]
];

RegisterPlot["Potential3D", "Potential Landscape 3D", "Heavy", 
  Function[{m}, QED`Plots`PlotPotentialSlices3D[m]]
];

(* 
   COMPUTE WORKER + SYNC
   1. Подменяем $CurrentModel на локальную.
   2. Запускаем "прогрев" кэша через GetNumericalQuantity (которая пишет в $CurrentModel).
   3. КОПИРУЕМ обновленный кэш из $CurrentModel обратно в локальную переменную.
   4. Строим график.
*)
SetAttributes[ComputePlotData, HoldFirst];
ComputePlotData[plotId_, model_Symbol] := 
  Block[{$CurrentModel = model},
    
    (* 1. ГАРАНТИРУЕМ ПРОГРЕВ КЭША *)
    (* Если кэш грязный, GetNumericalQuantity запустит ComputeNumericalHarmonicPerturbation,
       которая заполнит "ContinuationDerivatives" и "EquilibriumFluxes". 
       Без этого PlasmonFrequenciesVsFlux упадет. *)
    QED`Model`GetNumericalQuantity[$CurrentModel, "PlasmonFrequencies"];
    
    (* Для 3D графика нужны точки равновесия *)
    If[plotId === "Potential3D",
       QED`Model`GetNumericalQuantity[$CurrentModel, "EquilibriumPoints"]
    ];
    
    (* 2. СИНХРОНИЗАЦИЯ ОБРАТНО *)
    (* GetNumericalQuantity обновила $CurrentModel["Numerical"]. 
       Мы должны сохранить это в локальную model, чтобы IsDirty сбросился и данные сохранились. *)
    model["Numerical"] = $CurrentModel["Numerical"];
    
    (* 3. ВЫПОЛНЕНИЕ ОТРИСОВКИ *)
    Module[{info, func},
      info = $PlotRegistry[plotId];
      If[MissingQ[info], Return[Graphics[{Red, Text["Unknown Plot ID"]}]]];
      
      func = info["Compute"];
      (* Передаем $CurrentModel, так как она (и наша локальная model) теперь прогрета *)
      Check[func[$CurrentModel], Graphics[{Red, Text["Computation Failed"]}]]
    ]
  ];

(* ═══════════════════════════════════════════════════════════════ *)
(* 2. VIEW COMPONENTS: SLIDERS & CONTROLS *)
(* ═══════════════════════════════════════════════════════════════ *)

(* Извлечение интерактивных параметров из модели *)
ExtractInteractiveParams[model_Association] :=
  Flatten[
    KeyValueMap[
      Function[{tag, componentParams},
        KeyValueMap[
          (* Format: {Tag, ParamName, Value, {Min, Max, Step}} *)
          {tag, #1, #2["Value"], {#2["Min"], #2["Max"], #2["Step"]}} &,
          Select[componentParams, AssociationQ[#] && 
            Lookup[#, "Interactive", False] === True &]
        ]
      ],
      model["Primary"]
    ],
    1
  ];

(* Отрисовка одного слайдера с локальным Dynamic *)
(* ВАЖНО: HoldFirst нужен, чтобы model передавалась как символ, а не значение *)
SetAttributes[MakeParameterControl, HoldFirst];
MakeParameterControl[model_, {tag_, param_, val_, {min_, max_, step_}}, onUpdate_] := 
  Module[{currentVal = val},
    Row[{
      Style[param <> ": ", 12],
      
      (* Slider manipulates LOCAL variable inside Module *)
      Slider[
        Dynamic[
          model["Primary", tag, param, "Value"], 
          
          (* SETTER FUNCTION *)
          Function[{v},
            (* 1. Update Model (Reference via Symbol) *)
            model["Primary", tag, param, "Value"] = v;
            model["Numerical", "IsDirty"] = True;
            
            (* 2. Trigger Callback (for cache invalidation) *)
            onUpdate[]
          ]
        ],
        {min, max, step},
        ImageSize -> 120
      ],
      
      Spacer[5],
      
      (* InputField for precision *)
      InputField[
        Dynamic[
          model["Primary", tag, param, "Value"],
          Function[{v},
            model["Primary", tag, param, "Value"] = v;
            model["Numerical", "IsDirty"] = True;
            onUpdate[]
          ]
        ],
        Number, 
        FieldSize -> {5, 1}
      ]
    }]
  ];

(* Панель управления: Слайдеры + Кнопка обновления *)
(* ВАЖНО: HoldFirst нужен, чтобы пробросить символ model в MakeParameterControl *)
SetAttributes[PlotControlPanel, HoldFirst];
PlotControlPanel[model_, onUpdate_, onForceUpdate_] := 
  Module[{params},
    (* Здесь model ВЫЧИСЛЯЕТСЯ, чтобы получить список параметров. 
       Это нормально, нам нужны данные для построения UI. *)
    params = ExtractInteractiveParams[model];
    
    Column[
      Join[
        (* List of Sliders *)
        (* Map передает model (символ) в MakeParameterControl *)
        Map[
          MakeParameterControl[model, #, onUpdate] &,
          params
        ],
        
        {Spacer[10],
         (* Manual Update Button *)
         Button["Update Plot", 
           onForceUpdate[],
           Method -> "Queued",
           ImageSize -> {140, 30}
         ]}
      ]
    ]
  ];

(* ═══════════════════════════════════════════════════════════════ *)
(* 3. CORE: QUBIT DASHBOARD *)
(* ═══════════════════════════════════════════════════════════════ *)

QubitDashboard[modelsStack : {__Association}] := DynamicModule[
  {
    (* STATE *)
    currentModel = First[modelsStack],
    selectedPlotId = "PlasmonSpectrum",
    needsUpdate = False, (* Signal for Heavy plots *)
    
    (* CACHE (Transient) *)
    plotCache
  },
  
  (* VIEW LAYOUT *)
  Column[{
    Row[{
      (* LEFT PANEL: Controls *)
      Panel[
        Column[{
          (* Model Selector *)
          Style["Model: " <> ToString[currentModel["Topology"]["Name"]], Bold],
          Spacer[10],
          
          (* Sliders *)
          (* Здесь currentModel передается как символ благодаря HoldFirst у PlotControlPanel *)
          PlotControlPanel[
            currentModel, 
            
            (* onUpdate: Invalidate Cache *)
            Function[{}, 
               (* If Light plot -> Auto-update immediately *)
               If[$PlotRegistry[selectedPlotId]["Type"] === "Light",
                  needsUpdate = True,
                  (* If Heavy -> Just clear cache *)
                  plotCache[selectedPlotId] = Missing["Stale"]
               ]
            ],
            
            (* onForceUpdate: Button Click *)
            Function[{}, needsUpdate = True]
          ]
        }],
        Alignment -> Top
      ],
      
      Spacer[20],
      
      (* RIGHT PANEL: Plot Area *)
      Column[{
        (* Plot Selector *)
        Row[{
           "Plot Type: ",
           PopupMenu[Dynamic[selectedPlotId], Keys[$PlotRegistry]]
        }],
        Spacer[10],
        
        (* PLOT RENDERER *)
        Dynamic[
          (* 1. Check if we need to compute *)
          If[needsUpdate,
             (* Compute *)
             Module[{res},
               (* Передаем currentModel как СИМВОЛ. Внутри она обновится (кэш). *)
               res = ComputePlotData[selectedPlotId, currentModel];
               plotCache[selectedPlotId] = res;
               needsUpdate = False;
             ]
          ];
          
          (* 2. Render *)
          Module[{cached},
            cached = plotCache[selectedPlotId];
            
            Switch[cached,
              _Graphics | _Graphics3D | _Legended, cached,
              
              Missing["Stale"], 
              Panel[Style["Parameters changed. Press Update.", Gray], ImageSize->{300,300}],
              
              _, (* Initial state or missing *)
              If[$PlotRegistry[selectedPlotId]["Type"] === "Light",
                 needsUpdate = True; "Computing...", (* Auto-start light plots *)
                 Panel[Style["Select plot to start", Gray], ImageSize->{300,300}]
              ]
            ]
          ],
          
          TrackedSymbols :> {needsUpdate, selectedPlotId, plotCache}
        ]
      }, Alignment -> Top]
    }, Alignment -> Top],
    
    (* DEBUG FOOTER *)
    Dynamic @ Row[{"Cache: ", Keys[plotCache], " | Update: ", needsUpdate}]
  }],
  
  (* CONFIGURATION *)
  UnsavedVariables :> {plotCache, needsUpdate},
  Initialization :> {
    plotCache = <||>;
    needsUpdate = True; (* Force first render *)
  },
  SynchronousInitialization -> False,
  SaveDefinitions -> False
];

End[];
EndPackage[];